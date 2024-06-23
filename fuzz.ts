import assert from "node:assert"
import { Program, Exp, Prim, Int, Bool, If, Void } from "./factory.js"
// TODO: (almost) everything in here should use delayed evaluation
// TODO: analyseControlFlow is slow for large inputs. Probably quadratic, cubic or even worse.
export function createProgram(): Program {
  return Program(undefined, randomInteger())
}
const ops = [() => "==" as const, () => ">" as const, () => "<" as const, () => ">=" as const, () => "<=" as const]
function randomInteger(recursiveWeight = 0.7): Exp {
    return choose([randomIf, () => Int(range(100000))], [recursiveWeight, 1])
}
function randomIf(): Exp {
    return If(randomBoolean(), randomInteger(0.5), randomInteger(0.5))
}
function randomBoolean(): Exp {
  return choose([randomLogicalop, () => Bool(true), () => Bool(false), randomBinop], [6 * 2, 1, 1, 5 * 2])
}
function randomLogicalop(): Exp {
  return Prim(choose([() => "and", () => "or"]), randomBoolean(), randomBoolean())
}
function randomBinop() {
  return Prim(choose(ops), Int(range(10)), Int(range(10)))
}

/** Returns an int between 0 and range - 1 */
function range(range: number) {
  return Math.floor(Math.random() * range)
}
function weightedRandom<T>(arr: (() => T)[], weights: number[]) {
  assert(arr.length === weights.length)
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
