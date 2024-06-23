import assert from "node:assert"
import { Program, Exp, Prim, Int, Bool, If, Let, Var, Void } from "./factory.js"
import { Type, intType, boolType, gensym } from "./core.js"
import { alist, AList } from "./structures.js"
// TODO: (almost) everything in here should use delayed evaluation
// analyseControlFlow is slow for large inputs. Probably quadratic, cubic or even worse.
//    to get around this for now, keep a blockcount, and reduce the probability of block-producing expressions when it is exceeded
type Env = AList<string, Type> | undefined
let blockCount = 0
export function createProgram(): Program {
  blockCount = 0
  return Program(randomIntegerExp(undefined))
}
const cmps = [() => "==" as const, () => ">" as const, () => "<" as const, () => ">=" as const, () => "<=" as const]
function randomIntegerExp(env: Env): Exp {
  const ints = findValues(env, intType).map(v => () => Var(v))
  return choose(
    [
      ...(ints.length ? [() => choose(ints)] : []),
      () => Int(range(100000)),
      () => randomArithmetic(env),
      () => randomIf(env, randomIntegerExp),
      () => randomLet(env, randomIntegerExp),
    ],
    [...(ints.length ? [2] : []), 2, blockCount > 8 ? 0.001 : 2.5, 1, 0.5]
  )
}
function randomBooleanExp(env: Env): Exp {
  const bools = findValues(env, boolType).map(v => () => Var(v))
  return choose(
    [
      ...(bools.length ? [() => choose(bools)] : []),
      () => randomLogicalop(env),
      () => Bool(true),
      () => Bool(false),
      () => randomComparison(env),
      () => randomIf(env, randomBooleanExp),
      () => randomLet(env, randomBooleanExp),
    ],
    [...(bools.length ? [2] : []), 6 * 2, 2, 2, 5 * 2, blockCount > 8 ? 0.001 : 2.5, 0.5]
  )
}
function randomIf(env: Env, target: (env: Env) => Exp): Exp {
  blockCount += 2
  return If(randomBooleanExp(env), target(env), target(env))
}
function randomLet(env: Env, target: (env: Env) => Exp): Exp {
  const name = gensym()
  const [exp, i] = chooseAndTell([() => randomIntegerExp(env), () => randomBooleanExp(env)])
  const envp = alist(name, i === 0 ? intType : boolType, env)
  return Let(name, exp, target(envp))
}
function randomLogicalop(env: Env): Exp {
  return Prim(choose([() => "and", () => "or"]), randomBooleanExp(env), randomBooleanExp(env))
}
function randomComparison(env: Env): Exp {
  return Prim(choose(cmps), randomIntegerExp(env), randomIntegerExp(env))
}
function randomArithmetic(env: Env) {
  return choose(
    [
      () => Prim(choose([() => "+", () => "-"]), randomIntegerExp(env), randomIntegerExp(env)),
      () => Prim("-", randomIntegerExp(env)),
    ],
    [2, 1]
  )
}

/** Returns an int between 0 and range - 1 */
function range(range: number) {
  return Math.floor(Math.random() * range)
}
function weightedRandom<T>(arr: (() => T)[], weights: number[]) {
  assert(arr.length === weights.length, "array and weights must be the same length")
  const total = weights.reduce((sum, w) => sum + w, 0)
  let r = Math.random() * total
  for (let i = 0; i < arr.length; i++) {
    r -= weights[i]
    if (r < 0) return arr[i]()
  }
  return arr[arr.length - 1]()
}
function choose<T>(arr: (() => T)[], weights?: number[]): T {
  return weights ? weightedRandom(arr, weights) : arr[range(arr.length)]()
}
function chooseAndTell<T>(arr: (() => T)[]): [T, number] {
  const i = range(arr.length)
  return [arr[i](), i]
}
function findValues(env: Env, type: Type): string[] {
  const result = []
  while (env) {
    if (env.value === type) {
      result.push(env.key)
    }
    env = env.next
  }
  return result
}