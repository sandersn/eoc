import { assertDefined, read } from "./core.js"
import { Graph, DirectedGraph } from "./structures.js"
import { Var, Ref, Imm, Reg, Deref, X86Program, Instr, Block, Jmp, Ret, Cc, ByteReg, equalRef } from "./factory.js"
import assert from "node:assert"
function frameStackSize(homes: Map<string, Reg | Deref>): number {
  let stackLocations: Set<number> = new Set()
  let calleeLocations: Set<string> = new Set()
  for (const loc of homes.values()) {
    if (loc.kind === "deref") {
      stackLocations.add(loc.offset)
    } else if (loc.reg in callee) {
      calleeLocations.add(loc.reg)
    }
  }
  // round a number up to the nearest 16 bytes
  return Math.ceil((8 * stackLocations.size + 8 * calleeLocations.size) / 16) * 16 - 8 * calleeLocations.size
}
// callee save, if you reach these you have to push them in the prelude
const callee = {
  rbx: 7,
  rsp: -2,
  rbp: -3,
  r12: 8,
  r13: 9,
  r14: 10,
  r15: -5,
}
// initialise mapping of registers to numbers
// TODO: Compose these from callee/caller-save register objects
const numbers = {
  ...callee,
  rcx: 0,
  rdx: 1,
  rsi: 2,
  rdi: 3,
  r8: 4,
  r9: 5,
  r10: 6,
  rax: -1,
  r11: -4, // caller save (but I don't think you have to do anything; the caller will have to push them if they have used them, but they wouldn't)
}
const registers = ["rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "rbx", "r12", "r13", "r14"]
registers[-1] = "rax"
registers[-2] = "rsp"
registers[-3] = "rbp"
registers[-4] = "r11"
registers[-5] = "r15"
export function emitPreludeConclusion(p: X86Program): void {
  const stackSize = frameStackSize(p.info.homes)
  p.blocks.set(
    "main",
    Block({ references: [] }, [
      Instr("pushq", Reg("rbp")),
      Instr("movq", Reg("rsp"), Reg("rbp")),
      Instr("subq", Imm(stackSize), Reg("rsp")),
      Jmp("start"),
    ])
  )
  p.blocks.set(
    "conclusion",
    Block({ references: [] }, [Instr("addq", Imm(stackSize), Reg("rsp")), Instr("popq", Reg("rbp")), Ret()])
  )
}
export function patchInstructions(p: X86Program): void {
  for (const [name, block] of p.blocks) {
    p.blocks.set(name, Block(block.info, block.instructions.flatMap(patchInstructionsInstr)))
  }
}
function patchInstructionsInstr(i: Instr): Instr[] {
  switch (i.kind) {
    case "instr":
      if (i.op === "set") {
        return [i]
      }
      if (i.args.length === 2 && i.args[0].kind === "deref" && i.args[1].kind === "deref") {
        return [
          Instr("movq", i.args[0], { kind: "reg", reg: "rax" }),
          Instr(i.op, { kind: "reg", reg: "rax" }, i.args[1]),
        ]
      } else if (i.op === "cmpq" && i.args[1].kind === "imm") {
        return [
          Instr("movq", i.args[1], { kind: "reg", reg: "rax" }),
          Instr("cmpq", i.args[0], { kind: "reg", reg: "rax" }),
        ]
      } else if (i.op === "movzbq" && i.args[1].kind !== "reg") {
        return [
          Instr("movq", i.args[1], { kind: "reg", reg: "rax" }),
          Instr("movzbq", i.args[0], { kind: "reg", reg: "rax" }),
        ]
      } else if (i.op === "movq" && i.args.length === 2 && equalRef(i.args[0], i.args[1])) {
        return []
      }
    // fall through
    case "callq":
    case "ret":
    case "jmp":
    case "jmpif":
      return [i]
  }
}
export function buildInterference(p: X86Program): void {
  for (const [_, block] of p.blocks) {
    interferenceBlock(block, p.info.conflicts)
  }
}
function interferenceBlock(block: Block, conflicts: Graph<string>): void {
  for (let i = 0; i < block.instructions.length; i++) {
    const instr = block.instructions[i]
    const live = block.info.references[i + 1]
    if (instr.kind === "instr" && (instr.op === "movq" || instr.op === "movzbq")) {
      for (const v of live) {
        const source = nameOfRef(instr.args[0])
        const target = nameOfRef(instr.args[1])
        if (target && v !== source && v !== target) {
          conflicts.addEdge(target, v)
        }
      }
    } else {
      for (const d of liveWriteInstr(instr)) {
        for (const v of live) {
          if (v !== d) {
            conflicts.addEdge(d, v)
          }
        }
      }
    }
  }
}
export function uncoverLive(p: X86Program): void {
  const cfg = buildControlFlow(p.blocks)
  // not sure whether cfg.sort() is needed before, or after, cfg.transpose()
  cfg.g = cfg.transpose()
  // const labelToLive = new Map<string, Set<string>>([["conclusion", new Set(["rax", "rsp"])]])
  const transfer = (name: string, block: Set<string>, mapping: Map<string, Set<string>>): Set<string> => {
    // TODO: Can't tell when `block` is supposed to be used
    // Problem: p.blocks only contains start, but every `return` jmps to conclusion. p.blocks needs to have a conclusion at an earlier date
    // (orrr, have a special case in liveBlock to just return { rax, rsp } for conclusion
    if (name === 'conclusion') {
      return new Set(['rax', 'rsp'])
    }
    const b = assertDefined(p.blocks.get(name))
    const { after, references } = liveBlock(name, b.instructions, mapping)
    b.info.references = references
    return after
  }
  analyseControlFlow(cfg, transfer, new Set(), (s1, s2) => new Set([...s1, ...s2]))
}
function analyseControlFlow(
  g: DirectedGraph<string>,
  transfer: (name: string, block: Set<string>, mapping: Map<string, Set<string>>) => Set<string>,
  bottom: Set<string>,
  join: (s1: Set<string>, s2: Set<string>) => Set<string>
) {
  const mapping: Map<string, Set<string>> = new Map()
  for (const v of g.vertices()) {
    mapping.set(v, bottom)
  }
  const worklist = Array.from(g.vertices())
  const transg: DirectedGraph<string> = new DirectedGraph()
  transg.g = g.transpose()
  while (worklist.length) {
    const node = worklist.pop()!
    let input: Set<string> = new Set()
    for (const pred of transg.neighbours(node)) {
      input = join(input, assertDefined(mapping.get(pred)))
    }
    const output = transfer(node, input, mapping)
    if (!setEqual(output, assertDefined(mapping.get(node)))) {
      mapping.set(node, output)
      worklist.unshift(...g.neighbours(node))
    }
  }
}
function setEqual<T>(s1: Set<T>, s2: Set<T>) {
  for (const x of s1) {
    if (!s2.has(x)) return false
  }
  for (const x of s2) {
    if (!s1.has(x)) return false
  }
  return true
}
function buildControlFlow(blocks: Map<string, Block>): DirectedGraph<string> {
  const flow: DirectedGraph<string> = new DirectedGraph()
  for (const [name, block] of blocks) {
    for (const i of block.instructions) {
      if (i.kind === "jmp" || i.kind === "jmpif") {
        flow.addEdge(name, i.label)
      }
    }
  }
  return flow
}
function liveBlock(name: string, instructions: Instr[], labelToLive: Map<string, Set<string>>): { after: Set<string>, references: Set<string>[] } {
  let after: Set<string> = new Set()
  let prev: { kind: "jmp"; label: string } | undefined = undefined
  const references = [after]
  for (const i of instructions.toReversed()) {
    let before: Set<string>
    if (i.kind === "jmp") {
      before = labelToLive.get(i.label)!
      prev = i
    } else if (i.kind === "jmpif") {
      before = new Set([...labelToLive.get(i.label)!, ...labelToLive.get(assertDefined(prev).label)!])
    } else {
      const reads = liveReadInstr(i)
      const writes = liveWriteInstr(i)
      before = new Set([...Array.from(after).filter(live => !writes.includes(live)), ...reads])
    }
    references.push(before)
    after = before
  }
  return { after, references: references.toReversed() }
}
function liveReadInstr(i: Instr): string[] {
  switch (i.kind) {
    case "instr":
      switch (i.op) {
        case "movq":
        case "popq":
        case "negq":
          return [i.args[0]].map(nameOfRef).filter((s): s is string => s !== undefined)
        case "addq":
        case "subq":
        case "xorq":
        case "cmpq":
          return i.args.map(nameOfRef).filter((s): s is string => s !== undefined)
        case "pushq":
        case "set":
          // I don't think eflags doesn't need to be in the live graph because
          // 1. only cmpq can write to it
          // 2. only set and jmpif can read from it
          // So no interference problems.
          return []
        case "movzbq":
          // reads from rax qua al/ah
          return [nameOfRef(i.args[0]), nameOfByteRegContainer(i.args[0])].filter((s): s is string => s !== undefined)
      }
    case "callq":
    // TODO: callq reads from all caller-saved registers
    case "ret":
    case "jmp":
    case "jmpif":
      return []
  }
}
function liveWriteInstr(i: Instr): string[] {
  switch (i.kind) {
    case "instr":
      switch (i.op) {
        case "addq":
        case "subq":
        case "xorq":
        case "movq":
        case "movzbq":
          return [i.args[1]].map(nameOfRef).filter((s): s is string => s !== undefined)
        case "pushq":
        case "negq":
          return [i.args[0]].map(nameOfRef).filter((s): s is string => s !== undefined)
        case "cmpq":
        case "popq":
          return []
        case "set":
          // writes to rax as al/ah
          return [nameOfRef(i.args[1]), nameOfByteRegContainer(i.args[1])].filter((s): s is string => s !== undefined)
      }
    case "callq":
    // TODO: callq writes to all callee-saved registers
    case "ret":
    case "jmp":
    case "jmpif":
      return []
  }
}
export function allocateRegisters(p: X86Program): void {
  // replace all variables with registers, similar to assignHomes
  //   TODO: repurpose assignHomes/Ref to do this, but pass in homes instead of locals
  //   Best way to do this is to rewrite assignHomes to use homes, and preprocess locals to do that, then test with existing tests
  for (const [_, block] of p.blocks) {
    allocateRegisterBlock(block, p.info.conflicts, p.info.homes)
  }
  assignHomes(p)
}

function allocateRegisterBlock(b: Block, conflicts: Graph<string>, homes: Map<string, Reg | Deref>): void {
  // get vertices = variables + registers (I think there is a list NOT in the conflicts graph)
  //   NOTE: conflicts doesn't include everything, but it does include everything that will be used in the algorithm, so it's good enough
  // TODO: It also includes registers, which doesn't make sense; there's got to be a better starting point.
  const vertices = new Set(conflicts.g.keys())
  // initialise mapping with variables, unset and registers, set to their mapping
  //  (you can build the saturation list dynamically even though it's inefficient; it's just the assignments of the neighbours)
  //  that's why the value type isn't a tuple
  const colour: Map<string, number> = new Map()
  for (const v of vertices) {
    if (v in numbers) {
      colour.set(v, numbers[v as keyof typeof numbers])
      vertices.delete(v)
    }
  }
  // run DSATUR what a name
  while (vertices.size) {
    const u = maxBy(vertices, v => (colour.get(v) === undefined ? saturation(conflicts.g.get(v)!) : 0))
    if (!u) break
    let c = 0
    for (const n of Array.from(conflicts.g.get(u)!)
      .map(n => colour.get(n))
      .toSorted()) {
      if (n === undefined) break // quick exit; done with the already-assigned neighbours
      if (n === c) c++
    }
    colour.set(u, c)
    vertices.delete(u)
  }
  // compose mapping of variable/register->number and number->register/stack to get variable/register->register/stack
  for (const [varreg, c] of colour) {
    homes.set(varreg, c in registers ? Reg(registers[c]) : Deref("rbp", -8 * c))
  }

  function maxBy<T>(it: Iterable<T>, f: (t: T) => number): T | undefined {
    let best: T | undefined
    let bestVal = Number.MIN_SAFE_INTEGER
    for (const x of it) {
      const val = f(x)
      if (val > bestVal) {
        bestVal = val
        best = x
      }
    }
    return best
  }
  function saturation(neighbours: Iterable<string>): number {
    let n = 0
    for (const neighbour of neighbours) {
      if (colour.get(neighbour) !== undefined) n++
    }
    return n
  }
}
function assignHomes(p: X86Program): void {
  for (const [name, block] of p.blocks) {
    p.blocks.set(name, assignHomesBlock(block, p.info.homes))
  }
}
function assignHomesBlock(block: Block, homes: Map<string, Reg | Deref>): Block {
  return Block(
    block.info,
    block.instructions.map(i => assignHomesInstr(homes, i))
  )
}
function assignHomesInstr(info: Map<string, Deref | Reg>, i: Instr): Instr {
  switch (i.kind) {
    case "instr":
      if (i.op === "set") return Instr(i.op, i.args[0], assignHomesRef(info, i.args[1]))
      return Instr(i.op, ...i.args.map(a => assignHomesRef(info, a)))
    case "callq":
    case "ret":
    case "jmp":
    case "jmpif":
      return i
  }
}
function assignHomesRef(info: Map<string, Reg | Deref>, a: Ref): Ref {
  switch (a.kind) {
    case "imm":
    case "reg":
    case "deref":
    case "bytereg":
      return a
    case "var":
      return assertDefined(info.get(a.name))
  }
}
export function emitProgram(xp: X86Program): string {
  return Array.from(xp.blocks.entries())
    .map(([name, b]) => emitBlock(name, b))
    .join("\n")
}
function emitBlock(name: string, block: Block) {
  return `\t${name}:\n${block.instructions.map(emitInstr).join("\n")}`
}
function emitInstr(e: Instr): string {
  switch (e.kind) {
    case "instr":
      if (e.op === "set") return `${e.op}${e.args[0]} ${emitRef(e.args[1])}`
      return `${e.op} ${e.args.map(emitRef).join(", ")}`
    case "callq":
      return `callq ${e.label}`
    case "ret":
      return "ret"
    case "jmp":
      return `jmp ${e.label}`
    case "jmpif":
      return `j${e.cc} ${e.label}`
  }
}
function emitRef(r: Ref): string {
  switch (r.kind) {
    case "imm":
      return `$${r.int}`
    case "var":
      return r.name
    case "reg":
      return `%${r.reg}`
    case "bytereg":
      return `%${r.bytereg}`
    case "deref":
      return `${r.offset}(%${r.reg})`
  }
}
export function interpProgram(xp: X86Program) {
  const stack: number[] = []
  const registers: Map<string, number> = new Map()
  // TODO: Technically, byteregisters are halves of rax, rbx, rcx, rdx
  // but they're not used in an overlapping way as far as I know
  const byteregisters: Map<string, number> = new Map()
  const eflags = { left: 0, right: 0 }
  let blocks: Map<string, Block> = xp.blocks
  let block: Block | undefined = xp.blocks.get("main")
  while (block) {
    const result = interpBlock(block)
    if (result !== "jmp") {
      break
    }
  }
  return registers.get("rax")!

  function interpBlock(e: Block): "jmp" | undefined {
    for (const i of e.instructions) {
      const result = interpInstr(i)
      if (result === "jmp") {
        return result
      } else if (result === "ret") {
        return
      }
    }
  }
  function interpInstr(e: Instr): "ret" | "jmp" | undefined {
    const rsp = { kind: "reg", reg: "rsp" } as const
    switch (e.kind) {
      case "instr": {
        switch (e.op) {
          case "movq":
            return write(e.args[1], interpRef(e.args[0]))
          case "addq":
            return write(e.args[1], interpRef(e.args[1]) + interpRef(e.args[0]))
          case "subq":
            return write(e.args[1], interpRef(e.args[1]) - interpRef(e.args[0]))
          case "negq":
            return write(e.args[0], -interpRef(e.args[0]))
          case "xorq":
            return write(e.args[1], interpRef(e.args[1]) ^ interpRef(e.args[0]))
          case "cmpq":
            eflags.left = interpRef(e.args[1]) // cmpq is backwards on purpose!
            eflags.right = interpRef(e.args[0])
            return
          case "set":
            // TODO: Should maybe assert that args[1] is a bytereg
            write(e.args[1], compare(e.args[0]) ? 1 : 0)
            return
          case "movzbq":
            // TODO: Should maybe assert that args[0] is a bytereg
            return write(e.args[1], interpRef(e.args[0]))
          case "popq":
            write(e.args[0], stack[interpRef(rsp)])
            write(rsp, interpRef(rsp) + 8)
            return
          case "pushq":
            write(rsp, interpRef(rsp) - 8)
            stack[interpRef(rsp)] = interpRef(e.args[0])
            return
          default:
            throw new Error("unexpected instruction: " + (e as any).op)
        }
      }
      case "callq":
        // callq is only used for `read` right now
        registers.set("rax", read())
        return
      case "ret":
        return "ret"
      case "jmp":
        block = blocks.get(e.label)
        return "jmp"
      case "jmpif":
        if (compare(e.cc)) {
          block = blocks.get(e.label)
          return "jmp"
        }
        return
    }
  }
  function compare(cc: Cc) {
    switch (cc) {
      case "e":
        return eflags.left === eflags.right
      case "l":
        return eflags.left < eflags.right
      case "le":
        return eflags.left <= eflags.right
      case "g":
        return eflags.left > eflags.right
      case "ge":
        return eflags.left >= eflags.right
      default:
        throw new Error("unexpected compare: " + cc)
    }
  }
  function interpRef(r: Ref): number {
    switch (r.kind) {
      case "imm":
        return r.int
      case "var":
        return registers.get(r.name) ?? 0
      case "reg":
        return registers.get(r.reg) ?? 0
      case "bytereg":
        return byteregisters.get(r.bytereg) ?? 0
      case "deref":
        return assertDefined(stack[(registers.get(r.reg) ?? 0) + r.offset])
    }
  }
  function write(r: Ref, source: number): undefined {
    switch (r.kind) {
      case "var":
        registers.set(r.name, source)
        break
      case "reg":
        registers.set(r.reg, source)
        break
      case "bytereg":
        // TODO: Actually, this should be the bottom/top of a/b/c/d, but my generated
        // code should never expose this fact
        byteregisters.set(r.bytereg, source)
        break
      case "deref":
        stack[(registers.get(r.reg) ?? 0) + r.offset] = source
        break
      case "imm":
        throw new Error("not a ref")
    }
  }
}
function nameOfRef(r: Ref): string | undefined {
  switch (r.kind) {
    case "imm":
      return undefined
    case "var":
      return r.name
    case "bytereg":
      return r.bytereg
    case "reg":
    case "deref":
      return r.reg
  }
}
function nameOfByteRegContainer(r: Ref) {
  assert(r.kind === "bytereg")
  switch (r.bytereg) {
    case "ah":
    case "al":
      return "rax"
    case "bh":
    case "bl":
      return "rbx"
    case "ch":
    case "cl":
      return "rcx"
    case "dh":
    case "dl":
      return "rdx"
    default:
      throw new Error("Unexpected bytereg " + r.bytereg)
  }
}
