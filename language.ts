import { assertDefined, zip, unzip } from "./core.js"
import { Exp, Prim, Var, Program, Let, Stmt, Assign, Seq, Return, If, Bool } from "./factory.js"
import parse from "./parser.js"
import { AList } from "./structures.js"
import { read } from "./core.js"
let counter = 0
function gensym() {
  return "g" + counter++
}
/* ### Interpreter ### */
export function interpExp(e: Exp, env: AList<string, number>): number {
  switch (e.kind) {
    case "int":
      return e.val
    case "bool":
      return e.val ? 1 : 0
    case "var": {
      return assertDefined(env.get(e.name))
    }
    case "let":
      return interpExp(e.body, new AList(e.name, interpExp(e.exp, env), env))
    case "prim":
      return interpPrim(e, env)
    case "if":
      return interpExp(e.cond, env) === 1 ? interpExp(e.then, env) : interpExp(e.else, env)
  }
}
function interpPrim(e: Prim, env: AList<string, number>): number {
  if (e.op === "read") return +read()
  if (e.op === "+") return interpExp(e.args[0], env) + interpExp(e.args[1], env)
  if (e.op === "-" && e.args.length === 1) return -interpExp(e.args[0], env)
  if (e.op === "-" && e.args.length === 2) return interpExp(e.args[0], env) - interpExp(e.args[1], env)
  if (e.op === "and" && e.args.length === 2) return interpExp(e.args[0], env) && interpExp(e.args[1], env)
  if (e.op === "or" && e.args.length === 2) return interpExp(e.args[0], env) || interpExp(e.args[1], env)
  if (e.op === "not" && e.args.length === 1) return interpExp(e.args[0], env) ? 0 : 1
  if (e.op === "<=" && e.args.length === 2) return interpExp(e.args[0], env) <= interpExp(e.args[1], env) ? 1 : 0
  if (e.op === "<" && e.args.length === 2) return interpExp(e.args[0], env) < interpExp(e.args[1], env) ? 1 : 0
  if (e.op === ">=" && e.args.length === 2) return interpExp(e.args[0], env) >= interpExp(e.args[1], env) ? 1 : 0
  if (e.op === ">" && e.args.length === 2) return interpExp(e.args[0], env) > interpExp(e.args[1], env) ? 1 : 0
  if (e.op === "==" && e.args.length === 2) return interpExp(e.args[0], env) === interpExp(e.args[1], env) ? 1 : 0
  return NaN
}
export function interpProgram(p: Program): number {
  return interpExp(p.body, new AList("!!!!!!", NaN, undefined))
}

export function parseProgram(sexp: string): Program {
  return Program(new AList("!!!!!!!!!", NaN, undefined), parse(sexp))
}
/* ### Type checker ### */
/** It's nominal babyyyyyyyyyyyyy */
type Type = symbol
const intType = Symbol("Integer")
const boolType = Symbol("Boolean")
const operatorTypes = new Map<string, [[...Type[]], Type]>([
  ["+", [[intType, intType], intType]],
  ["-", [[intType, intType], intType]],
  ["read", [[], intType]],
  ["and", [[boolType, boolType], boolType]],
  ["or", [[boolType, boolType], boolType]],
  ["not", [[boolType], boolType]],
  ["<=", [[intType, intType], boolType]],
  ["<", [[intType, intType], boolType]],
  [">=", [[intType, intType], boolType]],
  [">", [[intType, intType], boolType]],
])
function isTypeEqual(t1: Type, t2: Type): boolean {
  return t1 === t2
}
function assertTypeEqual(got: Type, expected: Type, e: Exp): void {
  if (!isTypeEqual(got, expected)) {
    // TODO: Stop throwing errors and collect them instead
    throw new Error(`Got ${emitType(got)} but expected ${emitType(expected)} in ${emitExp(e)}`)
  }
}
/** I have NO idea why the type checker is allowed to return a modified tree (but in chapter 4 doesn't even use the capability yet). */
export function typeCheckProgram(p: Program): Program {
  const [body, t] = typeCheckExp(p.body, new AList("!!!!!!", intType, undefined))
  assertTypeEqual(t, intType, p.body)
  return Program(p.info, body)
}
function typeCheckExp(e: Exp, env: AList<string, Type>): [Exp, Type] {
  switch (e.kind) {
    case "int":
      return [e, intType]
    case "bool":
      return [e, boolType]
    case "prim": {
      if (e.op === "==") {
        // == is generic
        const [e1, t1] = typeCheckExp(e.args[0], env)
        const [e2, t2] = typeCheckExp(e.args[1], env)
        assertTypeEqual(t1, t2, e)
        return [Prim(e.op, e1, e2), boolType]
      } else if (e.op === "-" && e.args.length === 1) {
        // - is overloaded
        const [e1, t1] = typeCheckExp(e.args[0], env)
        assertTypeEqual(t1, intType, e)
        return [Prim(e.op, e1), intType]
      }
      const [args, ts] = unzip(e.args.map(arg => typeCheckExp(arg, env)))
      return [Prim(e.op, ...args), typeCheckOp(e.op, ts, e)]
    }
    case "var":
      return [e, assertDefined(env.get(e.name))]
    case "let": {
      const [exp, te] = typeCheckExp(e.exp, env)
      const [body, tbody] = typeCheckExp(e.body, new AList(e.name, te, env))
      return [Let(e.name, exp, body), tbody]
    }
    case "if": {
      const [cond, t1] = typeCheckExp(e.cond, env)
      const [then, t2] = typeCheckExp(e.then, env)
      const [else_, t3] = typeCheckExp(e.else, env)
      assertTypeEqual(t1, boolType, e.cond)
      assertTypeEqual(t3, t2, e)
      return [If(cond, then, else_), t2]
    }
  }
}
function typeCheckOp(op: string, args: Type[], e: Prim): Type {
  const sig = operatorTypes.get(op)
  if (sig) {
    const [params, ret] = sig
    for (const [arg, param] of zip(args, params)) {
      assertTypeEqual(arg, param, e)
    }
    return ret
  } else {
    throw new Error(`Unknown operator ${op}`)
  }
}
/* ### Emitter ### */
export function emitProgram(p: Program): string {
  return emitExp(p.body)
}
export function emitExp(e: Exp): string {
  switch (e.kind) {
    case "int":
      return `${e.val}`
    case "bool":
      return e.val ? "#t" : "#f"
    case "prim":
      return `(${e.op} ${e.args.map(a => emitExp(a)).join(" ")})`
    case "var":
      return e.name
    case "let":
      return `(let (${e.name} ${emitExp(e.exp)}) ${emitExp(e.body)})`
    case "if":
      return `[if ${emitExp(e.cond)} ${emitExp(e.then)} ${emitExp(e.else)}]`
  }
}
export function emitType(t: Type): string {
  switch (t) {
    case intType:
      return "int"
    case boolType:
      return "bool"
  }
  throw new Error(`Unexpected type ${t.toString()}`)
}
/* ### Frontend passes ###
 * 1. reparsePrimitives
 * 2. removeComplexOperands
 * 3. uniquify
 * 4. explicateTail -- produces a C program, called from c.ts (and maybe should be there)
 */
/** Convert primitive nodes to ifs and calls as needed */
export function reparsePrimitives(p: Program): Program {
  return Program(p.info, reparsePrimitivesExp(p.body))
}
function reparsePrimitivesExp(e: Exp): Exp {
  switch (e.kind) {
    case "prim":
      if (e.op === "if")
        return If(reparsePrimitivesExp(e.args[0]), reparsePrimitivesExp(e.args[1]), reparsePrimitivesExp(e.args[2]))
      else if (e.op === "and") {
        return If(reparsePrimitivesExp(e.args[0]), reparsePrimitivesExp(e.args[1]), Bool(false))
      } else if (e.op === "or") {
        return If(reparsePrimitivesExp(e.args[0]), Bool(true), reparsePrimitivesExp(e.args[1]))
      }
      return Prim(e.op, ...e.args.map(reparsePrimitivesExp))
    // TODO: Later, if e.op not in primitive list, then create a Call
    case "var":
    case "int":
    case "bool":
      return e
    case "let":
      return Let(e.name, reparsePrimitivesExp(e.exp), reparsePrimitivesExp(e.body))
    case "if":
      return If(reparsePrimitivesExp(e.cond), reparsePrimitivesExp(e.then), reparsePrimitivesExp(e.else))
  }
}

export function removeComplexOperands(p: Program): Program {
  return Program(p.info, removeComplexOperandsExp(p.body))
}
function removeComplexOperandsExp(e: Exp): Exp {
  switch (e.kind) {
    case "var":
    case "int":
    case "bool":
      return e
    case "prim": {
      if (e.op === "read") return e
      if (e.args.length === 1) {
        const [tmp, tmps] = removeComplexOperandsAtom(e.args[0])
        return generateTmpLets(Prim(e.op, tmp), tmps)
      } else if (e.args.length === 2) {
        const [tmp1, tmps1] = removeComplexOperandsAtom(e.args[0])
        const [tmp2, tmps2] = removeComplexOperandsAtom(e.args[1])
        return generateTmpLets(Prim(e.op, tmp1, tmp2), [...tmps1, ...tmps2])
      } else {
        throw new Error("Unexpected number of arguments")
      }
    }
    case "if":
      return If(removeComplexOperandsExp(e.cond), removeComplexOperandsExp(e.then), removeComplexOperandsExp(e.else))
    case "let": {
      return Let(e.name, removeComplexOperandsExp(e.exp), removeComplexOperandsExp(e.body))
    }
  }
}
function removeComplexOperandsAtom(e: Exp): [Exp, Array<[string, Exp]>] {
  switch (e.kind) {
    case "var":
    case "int":
    case "bool":
      return [e, []]
    case "prim": {
      const tmp = gensym()
      if (e.op === "read") {
        return [Var(tmp), [[tmp, e]]]
      }
      if (e.args.length === 1) {
        const [e1, tmps] = removeComplexOperandsAtom(e.args[0])
        return [Var(tmp), [[tmp, Prim(e.op, e1)], ...tmps]]
      } else if (e.args.length === 2) {
        const [e1, tmps1] = removeComplexOperandsAtom(e.args[0])
        const [e2, tmps2] = removeComplexOperandsAtom(e.args[1])
        const tmps = [...tmps1, ...tmps2]
        return [Var(tmp), [[tmp, Prim(e.op, e1, e2)], ...tmps]]
      } else {
        throw new Error("Unexpected number of arguments")
      }
    }
    case "if":
      // chapter 4 notes that e.cond should *definitely* be an expression, so I'm going to leave all 3
      // as-is and see whether explicateTail will fix things up
      const tmp = gensym()
      return [
        Var(tmp),
        [
          [
            tmp,
            If(removeComplexOperandsExp(e.cond), removeComplexOperandsExp(e.then), removeComplexOperandsExp(e.else)),
          ],
        ],
      ]
    case "let": {
      const [e1, tmpsE] = removeComplexOperandsAtom(e.exp)
      const [body1, tmpsBody] = removeComplexOperandsAtom(e.body)
      const tmp = gensym()
      return [Var(tmp), [[tmp, Let(e.name, e1, generateTmpLets(body1, tmpsBody))], ...tmpsE]]
    }
  }
}

export function uniquifyProgram(p: Program): Program {
  return Program(p.info, uniquifyExp(p.body, new AList("!!!!!!", "@@@@@@@@@", undefined)))
}
function uniquifyExp(e: Exp, env: AList<string, string>): Exp {
  switch (e.kind) {
    case "var":
      return Var(assertDefined(env.get(e.name)))
    case "int":
    case "bool":
      return e
    case "prim":
      return Prim(e.op, ...e.args.map(a => uniquifyExp(a, env)))
    case "if":
      return If(uniquifyExp(e.cond, env), uniquifyExp(e.then, env), uniquifyExp(e.else, env))
    case "let": {
      let x = env.get(e.name) ? gensym() : e.name
      env = new AList(e.name, x, env)
      return Let(x, uniquifyExp(e.exp, env), uniquifyExp(e.body, env))
    }
  }
}
function generateTmpLets(init: Exp, tmps: Array<[string, Exp]>): Exp {
  return tmps.reduce((e, [name, exp]) => Let(name, exp, e), init)
}
