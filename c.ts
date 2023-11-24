import { assertDefined } from "./core.js"
import { Exp, Prim, Ref, Imm, Reg, Stmt, Program, CProgram, X86Program, Instr, Block, Callq } from "./factory.js"
import { interpExp, explicateTail, emitExp } from "./language.js"
import { DirectedGraph, AList } from "./structures.js"
/** Doesn't actually type check currently, but binds name to index */
function typeCheck(s: Stmt): Map<string, number> {
  const env: Map<string, number> = new Map()
  let i = 1
  function worker(s: Stmt): void {
    switch (s.kind) {
      case "assign":
        env.set(s.var.name, i)
        i++
        break
      case "return":
        break
      case "seq":
        for (const statement of s.statements) worker(statement)
        break
    }
  }
  worker(s)
  return env
}
export function selectInstructions(p: CProgram): X86Program {
  const start = assertDefined(p.body.get("start"))
  return {
    info: new Map(),
    blocks: new Map([
      [
        "start",
        Block(
          { homes: new Map(), references: [], conflicts: new DirectedGraph() },
          selectInstructionsStmt(assertDefined(start))
        ),
      ],
    ]),
  }
}
function selectInstructionsExp(e: Exp, to: Ref): Instr[] {
  switch (e.kind) {
    case "prim":
      return selectInstructionsPrim(e, to)
    case "int":
      return [Instr("movq", Imm(e.val), to)]
    case "bool":
      return [Instr("movq", Imm(e.val ? 1 : 0), to)]
    case "var":
      return [Instr("movq", e, to)]
    case "if":
    case "let":
      throw new Error(`Unexpected ${e.kind} on rhs of assignment.`)
  }
}
function selectInstructionsPrim(e: Prim, to: Ref): Instr[] {
  switch (e.op) {
    case "read":
      return [Callq("read_int", 0), Instr("movq", Reg("rax"), to)]
    case "+":
      return [
        Instr("movq", selectInstructionsAtom(e.args[0]), to),
        Instr("addq", selectInstructionsAtom(e.args[1]), to),
      ]
    case "-":
      if (e.args.length === 1) {
        return [Instr("movq", selectInstructionsAtom(e.args[0]), to), Instr("negq", to)]
      } else {
        return [
          Instr("movq", selectInstructionsAtom(e.args[0]), to),
          Instr("subq", selectInstructionsAtom(e.args[1]), to),
        ]
      }
    default:
      throw new Error("Unexpected primitive")
  }
}
function selectInstructionsStmt(s: Stmt): Instr[] {
  switch (s.kind) {
    case "assign":
      return selectInstructionsExp(s.exp, s.var)
    case "return":
      return selectInstructionsExp(s.exp, Reg("rax"))
    case "seq":
      return s.statements.flatMap(s => selectInstructionsStmt(s))
  }
}
function selectInstructionsAtom(e: Exp): Ref {
  switch (e.kind) {
    case "int":
      return Imm(e.val)
    case "bool":
      return Imm(e.val ? 1 : 0)
    case "var":
      return e
    case "prim":
    case "if":
    case "let":
      throw new Error("Unexpected non-atomic expression in selectInstructionsAtom")
  }
}
function interpStatement(e: Stmt, env: Map<string, number>): number {
  switch (e.kind) {
    case "assign": {
      const v = interpExp(e.exp, AList.fromMap(env))
      env.set(e.var.name, v)
      return v
    }
    case "seq":
      return e.statements.reduce((_, s) => interpStatement(s, env), 0)
    case "return":
      return interpExp(e.exp, AList.fromMap(env))
  }
}
export function interpProgram(p: CProgram): number {
  return interpStatement(assertDefined(p.body.get("start")), p.locals)
}
function emitStatement(e: Stmt): string {
  switch (e.kind) {
    case "assign":
      return `${e.var.name} = ${emitExp(e.exp)};\n`
    case "return":
      return `return ${emitExp(e.exp)};\n`
    case "seq":
      return e.statements.map(s => emitStatement(s)).join("")
  }
}
export function emitProgram(p: CProgram): string {
  return emitStatement(assertDefined(p.body.get("start")))
}
export function explicateControl(p: Program): CProgram {
  const start = explicateTail(p.body)
  return CProgram(
    typeCheck(start), // for now; the only block is :start; after that, storing ALL locals on the program doesn't make much sense
    new Map([["start", start]])
  )
}
