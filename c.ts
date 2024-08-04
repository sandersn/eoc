import { assertDefined, box, Value } from "./core.js"
import assert from "node:assert"
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
  Int,
  Reg,
  ByteReg,
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
  equalRef,
  Jmp,
  JmpIf,
  Cc,
} from "./factory.js"
import { interpExp, emitExp } from "./language.js"
import { Graph, alistFromMap } from "./structures.js"
/** TODO: Should maybe type check (but surely that's the responsiblity of the frontend?) */
function bind(blocks: [string, Stmt][]): Map<string, Value> {
  const env: Map<string, Value> = new Map()
  let i = 1
  function worker(s: Stmt): void {
    switch (s.kind) {
      case "assign":
        env.set(s.var.name, { kind: "int", value: i })
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
/**
 * ### IL stages ###
 * 1. explicateTail -- produces a C program from a Language program
 * 2. selectInstructions -- produces an x86-var program from a C program
 */
export function selectInstructions(p: CProgram): X86Program {
  const ret: X86Program = {
    info: {
      homes: new Map(),
      conflicts: new Graph(),
    },
    blocks: new Map([]),
  }
  for (const [name, stmt] of p.body) {
    const instructions = selectInstructionsStmt(stmt)
    ret.blocks.set(name, Block({ references: [] }, instructions))
  }
  return ret
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
    case "set":
    case "get":
    case "begin":
    case "while":
    case "as":
      throw new Error(`Unexpected ${e.kind} on rhs of assignment.`)
    case "allocate":
    case "collect":
    case "global-value":
      throw new Error("Don't know how to handle allocate/collect/global-value in explicateEffect.")
    case "void":
      return [Instr("movq", Imm(0), to)]
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
    case "not":
      const arg = selectInstructionsAtom(e.args[0])
      if (equalRef(to, arg)) {
        return [Instr("xorq", Imm(1), to)]
      }
      return [Instr("movq", arg, to), Instr("xorq", Imm(1), to)]
    case "==":
    case "<":
    case "<=":
    case ">":
    case ">=":
      return [
        Instr("cmpq", selectInstructionsAtom(e.args[1]), selectInstructionsAtom(e.args[0])),
        Instr("set", selectInstructionsOp(e.op), ByteReg("al")),
        Instr("movzbq", ByteReg("al"), to),
      ]
    default:
      throw new Error("Unexpected primitive")
  }
}
function selectInstructionsOp(op: string): Cc {
  switch (op) {
    case "==":
      return "e"
    case "<":
      return "l"
    case "<=":
      return "le"
    case ">":
      return "g"
    case ">=":
      return "ge"
    default:
      throw new Error("unexpected op " + op)
  }
}
function selectInstructionsStmt(s: Stmt): Instr[] {
  switch (s.kind) {
    case "assign":
      return selectInstructionsExp(s.exp, s.var)
    case "return":
      return [...selectInstructionsExp(s.exp, Reg("rax")), Jmp("conclusion")]
    case "goto":
      return [Jmp(s.label)]
    case "if":
      return [
        Instr("cmpq", selectInstructionsAtom(s.cond.args[1]), selectInstructionsAtom(s.cond.args[0])),
        JmpIf(selectInstructionsOp(s.cond.op), s.then.label),
        Jmp(s.else.label),
      ]
    case "seq":
      return [...selectInstructionsStmt(s.head), ...selectInstructionsStmt(s.tail)]
    case "void":
      throw new Error(`Unexpected ${s.kind} on rhs of assignment.`)
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
    case "set":
    case "get":
    case "begin":
    case "while":
    case "as":
      throw new Error(`Unexpected non-atomic expression ${e.kind} in selectInstructionsAtom`)
    case "allocate":
    case "collect":
    case "global-value":
      throw new Error("Don't know how to handle allocate/collect/global-value in explicateEffect.")
    case "void":
      return Imm(0)
  }
}
export function interpProgram(p: CProgram): number {
  const value = interpStatement(assertDefined(p.body.get("start")))
  assert(value.kind === "int")
  return value.value
  function interpStatement(e: Stmt): Value {
    const env2 = alistFromMap(p.locals, box)
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
        const cond = interpExp(e.cond, env2)
        return interpStatement(cond.kind === "int" && cond.value === 1 ? e.then : e.else)
      case "goto":
        return interpStatement(assertDefined(p.body.get(e.label)))
      case "return":
        return interpExp(e.exp, env2)
      case "void":
        return { kind: "void" }
    }
  }
}
function emitStatement(e: Stmt): string {
  switch (e.kind) {
    case "assign":
      return `\t${e.var.name} = ${emitExp(e.exp)};\n`
    case "return":
      return `\treturn ${emitExp(e.exp)};\n`
    case "seq":
      return emitStatement(e.head) + emitStatement(e.tail)
    case "goto":
      return `\tgoto ${e.label};\n`
    case "if":
      return `\tif ${emitExp(e.cond)} ${emitStatement(e.then)}\telse ${emitStatement(e.else)}`
    case "void":
      return "void;"
  }
}
export function emitProgram(p: CProgram): string {
  let emit = ""
  for (const [name, stmts] of p.body) {
    emit += `${name}:\n${emitStatement(stmts)}`
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
        k = createBlock(k)
        return explicatePred(e.cond, explicateAssign(e.then, x, k), explicateAssign(e.else, x, k))
      case "let": {
        return explicateAssign(e.exp, e.name, explicateAssign(e.body, x, k))
      }
      case "set":
        // TOD: Pretty sure e.exp needs to recur with an explicate* (same for other entries)
        return Seq(Assign(Var(x), Int(0)), Seq(Assign(Var(e.name), e.exp), k))
      case "get":
        throw new Error("removeComplexOperands should remove get")
      case "begin": {
        let body = explicateAssign(e.body, x, k)
        for (let i = e.exps.length - 1; i >= 0; i--) {
          body = explicateEffect(e.exps[i], body)
        }
        return body
      }
      case "while":
        const label = genLabel("block")
        const loop = Goto(label)
        const body = explicateEffect(e.body, loop)
        blocks.push([label, explicatePred(e.cond, body, k)])
        return Seq(Assign(Var(x), Int(0)), loop)
      case "void":
        return k
      case "as":
        throw new Error("as should have been removed by exposeAllocation")
      case "allocate":
      case "collect":
      case "global-value":
        throw new Error("Don't know how to handle allocate/collect/global-value in explicateEffect.")
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
          case "goto":
            return explicateAssign(e.exp, e.name, tail)
          case "if":
            return explicateAssign(e.exp, e.name, explicatePred(tail.cond, tail.then, tail.else))
          case "assign":
            throw new Error("Unexpected assign")
        }
      }
      case "set":
        // TODO: This is probably wrong (and should be prevented by the type checker in any case)
        return Assign(Var(e.name), e.exp)
      case "get":
        throw new Error("removeComplexOperands should remove get")
      case "begin":
        let body = explicateTail(e.body)
        for (let i = e.exps.length - 1; i >= 0; i--) {
          body = explicateEffect(e.exps[i], body)
        }
        return body
      case "while": {
        // TODO: This is probably wrong (and should be prevented by the type checker in any case)
        const label = genLabel("block")
        const loop = Goto(label)
        const body = explicateEffect(e.body, loop)
        blocks.push([label, explicatePred(e.cond, body, Return(Int(0)))])
        return loop
      }
      case "void":
        return Return(Int(0))
      case "as":
        throw new Error("as should have been removed by exposeAllocation")
      case "allocate":
      case "collect":
      case "global-value":
        throw new Error("Don't know how to handle allocate/collect/global-value in explicateTail.")
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
      case "begin": {
        let body = explicatePred(cond.body, then, else_)
        for (let i = cond.exps.length - 1; i >= 0; i--) {
          body = explicateEffect(cond.exps[i], body)
        }
        return body
      }
      case "set":
      case "get":
      case "while":
      case "void":
      case "as":
        throw new Error("Type checker should prevent set/get/while/void/as from appearing here")
      case "allocate":
      case "collect":
      case "global-value":
        throw new Error("Don't know how to handle allocate/collect/global-value in explicatePred.")
    }
  }
  function explicateEffect(e: Exp, k: Stmt): Stmt {
    switch (e.kind) {
      case "var":
      case "int":
      case "bool":
      case "prim":
      case "get":
        // TODO: Could instead return k to eliminate the pure code
        throw new Error("Should not have expressions used for their effects.")
      case "if":
        return explicatePred(e.cond, explicateEffect(e.then, k), explicateEffect(e.else, k))
      case "let":
        return explicateAssign(e.exp, e.name, explicateEffect(e.body, k))
      case "set":
        return explicateAssign(e.exp, e.name, k)
      case "begin": {
        let body = explicateEffect(e.body, k)
        for (let i = e.exps.length - 1; i >= 0; i--) {
          body = explicateEffect(e.exps[i], body)
        }
        return body
      }
      case "while":
        const label = genLabel("block")
        const loop = Goto(label)
        const body = explicateEffect(e.body, loop)
        blocks.push([label, explicatePred(e.cond, body, k)])
        return loop
      case "void":
        return k
      case "as":
        throw new Error("as should have been removed by exposeAllocation")
      case "allocate":
      case "collect":
      case "global-value":
        throw new Error("Don't know how to handle allocate/collect/global-value in explicateEffect.")
    }
  }
}
let counter = 0
function genLabel(prefix = "g") {
  return prefix + counter++
}
