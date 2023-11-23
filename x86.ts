import { assertDefined, read } from "./core.js"
import { DirectedGraph } from "./structures.js"
import { Var, Ref, Imm, Reg, Deref, X86Program, Instr, Block, Jmp, Ret } from "./factory.js"
function equalRef(r1: Ref, r2: Ref): boolean {
  if (r1.kind !== r2.kind) return false
  switch (r1.kind) {
    case "imm":
      return r1.int === (r2 as Imm).int
    case "reg":
      return r1.reg === (r2 as Reg).reg
    case "var":
      return r1.name === (r2 as Var).name
    case "deref":
      return r1.reg === (r2 as Deref).reg && r1.offset === (r2 as Deref).offset
  }
}

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
export function emitPreludeConclusion(p: X86Program): X86Program {
  const start = assertDefined(p.blocks.get("start"))
  start.instructions.push({ kind: "jmp", label: "conclusion" })
  const stackSize = frameStackSize(start.info.homes)
  p.blocks.set(
    "main",
    Block({ homes: new Map(), references: [], conflicts: new DirectedGraph() }, [
      Instr("pushq", Reg("rbp")),
      Instr("movq", Reg("rsp"), Reg("rbp")),
      Instr("subq", Imm(stackSize), Reg("rsp")),
      Jmp("start"),
    ])
  )
  p.blocks.set(
    "conclusion",
    Block({ homes: new Map(), references: [], conflicts: new DirectedGraph() }, [
      Instr("addq", Imm(stackSize), Reg("rsp")),
      Instr("popq", Reg("rbp")),
      Ret(),
    ])
  )
  return p
}
export function patchInstructions(p: X86Program): X86Program {
  const start = assertDefined(p.blocks.get("start"))
  return X86Program(
    p.info,
    p.blocks.set("start", Block(start.info, start.instructions.flatMap(patchInstructionsInstr)))
  )
}
function patchInstructionsInstr(i: Instr): Instr[] {
  switch (i.kind) {
    case "instr":
      if (i.args.length === 2 && i.args[0].kind === "deref" && i.args[1].kind === "deref") {
        return [
          Instr("movq", i.args[0], { kind: "reg", reg: "rax" }),
          Instr(i.op, { kind: "reg", reg: "rax" }, i.args[1]),
        ]
      } else if (i.op === "movq" && i.args.length === 2 && equalRef(i.args[0], i.args[1])) {
        return []
      }
    // fall through
    case "callq":
    case "ret":
    case "jmp":
      return [i]
  }
}
export function buildInterference(p: X86Program): void {
  interferenceBlock(assertDefined(p.blocks.get("start")))
}
function interferenceBlock(block: Block): void {
  for (let i = 0; i < block.instructions.length; i++) {
    const instr = block.instructions[i]
    const live = block.info.references[i + 1]
    if (instr.kind === "instr" && instr.op === "movq") {
      for (const v of live) {
        const source = nameOfRef(instr.args[0])
        const target = nameOfRef(instr.args[1])
        if (target && v !== source && v !== target) {
          block.info.conflicts.addEdge(target, v)
        }
      }
    } else {
      for (const d of liveWriteInstr(instr)) {
        for (const v of live) {
          if (v !== d) {
            block.info.conflicts.addEdge(d, v)
          }
        }
      }
    }
  }
}
export function uncoverLive(p: X86Program): X86Program {
  const start = assertDefined(p.blocks.get("start"))
  return X86Program(p.info, p.blocks.set("start", liveBlock(start, new Set(["rax", "rsp"]))))
}
function liveBlock(block: Block, initial: Set<string>): Block {
  let after = initial
  const references = [after]
  for (const i of block.instructions.toReversed()) {
    let before: Set<string>
    if (i.kind === "jmp") {
      before = initial
    } else {
      const reads = liveReadInstr(i)
      const writes = liveWriteInstr(i)
      before = new Set([...Array.from(after).filter(live => !writes.includes(live)), ...reads])
    }
    references.push(before)
    after = before
  }
  return Block({ ...block.info, references: references.toReversed() }, block.instructions)
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
          return i.args.map(nameOfRef).filter((s): s is string => s !== undefined)
        case "pushq":
          return []
      }
    case "callq":
    // TODO: callq reads from all caller-saved registers
    case "ret":
    case "jmp":
      return []
  }
}
function liveWriteInstr(i: Instr): string[] {
  switch (i.kind) {
    case "instr":
      switch (i.op) {
        case "addq":
        case "subq":
        case "movq":
          return [i.args[1]].map(nameOfRef).filter((s): s is string => s !== undefined)
        case "pushq":
        case "negq":
          return [i.args[0]].map(nameOfRef).filter((s): s is string => s !== undefined)
        case "popq":
          return []
      }
    case "callq":
    // TODO: callq writes to all callee-saved registers
    case "ret":
    case "jmp":
      return []
  }
}
export function allocateRegisters(p: X86Program): X86Program {
  const b = assertDefined(p.blocks.get("start"))
  // get vertices = variables + registers (I think there is a list NOT in the conflicts graph)
  //   NOTE: conflicts doesn't include everything, but it does include everything that will be used in the algorithm, so it's good enough
  // TODO: It also includes registers, which doesn't make sense; there's got to be a better starting point.
  const vertices = new Set(b.info.conflicts.g.keys())
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
    const u = maxBy(vertices, v => (colour.get(v) === undefined ? saturation(b.info.conflicts.g.get(v)!) : 0))
    if (!u) break
    let c = 0
    for (const n of Array.from(b.info.conflicts.g.get(u)!)
      .map(n => colour.get(n))
      .toSorted()) {
      if (n === undefined) break // quick exit; done with the already-assigned neighbours
      if (n === c) c++
    }
    colour.set(u, c)
    vertices.delete(u)
  }
  // compose mapping of variable/register->number and number->register/stack to get variable/register->register/stack
  const homes: Map<string, Reg | Deref> = new Map()
  for (const [varreg, c] of colour) {
    homes.set(varreg, c in registers ? Reg(registers[c]) : Deref("rbp", -8 * c))
  }
  b.info.homes = homes
  // replace all variables with registers, similar to assignHomes
  //   TODO: repurpose assignHomes/Ref to do this, but pass in homes instead of locals
  //   Best way to do this is to rewrite assignHomes to use homes, and preprocess locals to do that, then test with existing tests
  return assignHomes(p)

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
function assignHomes(p: X86Program): X86Program {
  const start = assertDefined(p.blocks.get("start"))
  return X86Program(p.info, p.blocks.set("start", assignHomesBlock(start)))
}
function assignHomesBlock(block: Block): Block {
  return Block(
    block.info,
    block.instructions.map(i => assignHomesInstr(block.info.homes, i))
  )
}
function assignHomesInstr(info: Map<string, Deref | Reg>, i: Instr): Instr {
  switch (i.kind) {
    case "instr":
      return Instr(i.op, ...i.args.map(a => assignHomesRef(info, a)))
    case "callq":
    case "ret":
    case "jmp":
      return i
  }
}
function assignHomesRef(info: Map<string, Reg | Deref>, a: Ref): Ref {
  switch (a.kind) {
    case "imm":
    case "reg":
    case "deref":
      return a
    case "var":
      return assertDefined(info.get(a.name))
  }
}
function emitProgram(xp: X86Program): string {
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
      return `${e.op} ${e.args.map(emitRef).join(", ")}`
    case "callq":
      return `callq ${e.label}`
    case "ret":
      return "ret"
    case "jmp":
      return `jmp ${e.label}`
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
    case "deref":
      return `${r.offset}(%${r.reg})`
  }
}
export function interpProgram(xp: X86Program) {
  const stack: number[] = []
  const registers: Map<string, number> = new Map()
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
          case "popq":
            write(e.args[0], stack[interpRef(rsp)])
            write(rsp, interpRef(rsp) + 8)
            return
          case "pushq":
            write(rsp, interpRef(rsp) - 8)
            stack[interpRef(rsp)] = interpRef(e.args[0])
            return
          default:
            throw new Error("unexpected instruction: " + e.op)
        }
      }
      case "callq":
        registers.set("rax", read())
        break
      case "ret":
        return "ret"
      case "jmp":
        block = blocks.get(e.label)
        return "jmp"
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
    case "reg":
    case "deref":
      return r.reg
  }
}
