import assert from "assert"
import { assertDefined } from "./core.js"
import {
  Assign,
  Var,
  Return,
  Seq,
  Exp,
  Cmp,
  Prim,
  Ref,
  Imm,
  Reg,
  Stmt,
  Program,
  CProgram,
  X86Program,
  Instr,
  Block,
  Callq,
  Goto,
  IfStmt,
  Bool,
} from "./factory.js"
import { interpExp, emitExp } from "./language.js"
import { DirectedGraph, AList } from "./structures.js"
/** TODO: Should maybe type check (but surely that's the responsiblity of the frontend?) */
function bind(blocks: [string, Stmt][]): Map<string, number> {
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
        worker(s.head)
        worker(s.tail)
        break
    }
  }
  for (const [_, stmt] of blocks) worker(stmt)
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
    case "goto":
      throw new Error("Don't know how to select instructions for goto")
    case "if":
      throw new Error("Don't know how to select instructions for if")
    case "seq":
      return [...selectInstructionsStmt(s.head), ...selectInstructionsStmt(s.tail)]
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
export function interpProgram(p: CProgram): number {
  return interpStatement(assertDefined(p.body.get("start")))
  function interpStatement(e: Stmt): number {
    const env2 = AList.fromMap(p.locals)
    switch (e.kind) {
      case "assign": {
        const v = interpExp(e.exp, env2)
        p.locals.set(e.var.name, v)
        return v
      }
      case "seq":
        interpStatement(e.head)
        return interpStatement(e.tail)
      case "if":
        return interpStatement(interpExp(e.cond, env2) ? e.then : e.else)
      case "goto":
        return interpStatement(assertDefined(p.body.get(e.label)))
      case "return":
        return interpExp(e.exp, env2)
    }
  }
}
function emitStatement(e: Stmt): string {
  switch (e.kind) {
    case "assign":
      return `${e.var.name} = ${emitExp(e.exp)};\n`
    case "return":
      return `return ${emitExp(e.exp)};\n`
    case "seq":
      return emitStatement(e.head) + emitStatement(e.tail)
    case "goto":
      return `goto ${e.label};\n`
    case "if":
      return `if ${emitExp(e.cond)} ${emitStatement(e.then)}else ${emitStatement(e.else)}\n`
  }
}
export function emitProgram(p: CProgram): string {
  let emit = ''
  for (const [name, stmts] of p.body) {
    emit += `\t${name}:\n${emitStatement(stmts)}`
  }
  return emit
}
export function explicateControl(p: Program): CProgram {
  const blocks: [string, Stmt][] = []
  const start = explicateTail(p.body)
  blocks.push(["start", start])
  return CProgram(bind(blocks), new Map(blocks))

  function explicateAssign(e: Exp, x: string, k: Stmt): Stmt {
    switch (e.kind) {
      case "var":
      case "int":
      case "bool":
      case "prim":
        return Seq(Assign(Var(x), e), k)
      case "if":
        // TODO: I just wrote this mechanically (or copilot did) but it works. Come up with more test cases to break it!
        k = createBlock(k)
        return explicatePred(e.cond, explicateAssign(e.then, x, k), explicateAssign(e.else, x, k))
      case "let": {
        return explicateAssign(e.exp, e.name, explicateAssign(e.body, x, k))
      }
    }
  }
  function explicateTail(e: Exp): Stmt {
    switch (e.kind) {
      case "var":
      case "int":
      case "bool":
      case "prim":
        return Return(e)
      case "if":
        return explicatePred(e.cond, explicateTail(e.then), explicateTail(e.else))
      case "let": {
        const tail = explicateTail(e.body)
        switch (tail.kind) {
          case "seq":
          case "return":
            return explicateAssign(e.exp, e.name, tail)
          case "goto":
            throw new Error("Checker should prevent goto from appearing here")
          case "if":
            return explicateAssign(e.exp, e.name, explicatePred(tail.cond, tail.then, tail.else))
          case "assign":
            throw new Error("Unexpected assign")
        }
      }
    }
  }
  function createBlock(k: Stmt): Goto {
    if (k.kind === "goto") return k
    const label = genLabel("block")
    blocks.push([label, k])
    return Goto(label)
  }
  function explicatePred(cond: Exp, then: Stmt, else_: Stmt): Stmt {
    switch (cond.kind) {
      case "var":
        return IfStmt(Prim("==", cond, Bool(true)), createBlock(then), createBlock(else_))
      case "int":
        throw new Error("Type checker should prevent numbers from appearing here")
      case "bool":
        return cond.val ? then : else_
      case "prim":
        switch (cond.op) {
          case "==":
          case ">":
          case "<":
          case ">=":
          case "<=":
            return IfStmt(cond as Cmp, createBlock(then), createBlock(else_))
          case "not":
            return explicatePred(cond.args[0], else_, then)
          case "and":
          case "or":
            throw new Error("and/or not expected in this pass of the compiler")
          default:
            throw new Error(`op ${cond.op} not expected in this pass of the compiler`)
        }
      case "if":
        then = createBlock(then)
        else_ = createBlock(else_)
        return explicatePred(cond.cond, explicatePred(cond.then, then, else_), explicatePred(cond.else, then, else_))
      case "let":
        return explicateAssign(cond.exp, cond.name, explicatePred(cond.body, then, else_))
    }
  }
}
let counter = 0
function genLabel(prefix = "g") {
  return prefix + counter++
}
