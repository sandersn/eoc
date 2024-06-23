import assert from "node:assert"
import { Program, Exp, Prim, Int, Bool, If, Void } from "./factory.js"
// TODO: (almost) everything in here should use delayed evaluation
// TODO: analyseControlFlow is slow for large inputs. Probably quadratic, cubic or even worse.
//    to get around this for now, keep a blockcount, and reduce the probability of block-producing expressions when it is exceeded
let blockCount = 0
export function createProgram(): Program {
  blockCount = 0
  return Program(undefined, randomIntegerExp())
}
const ops = [() => "==" as const, () => ">" as const, () => "<" as const, () => ">=" as const, () => "<=" as const]
function randomIntegerExp(): Exp {
  return choose([() => Int(range(100000)), randomIf(randomIntegerExp)], [1, blockCount > 8 ? 0.001 : 2.5])
}
function randomIf(target: () => Exp): () => Exp {
  blockCount += 2
  return () => If(randomBooleanExp(), target(), target())
}
function randomBooleanExp(): Exp {
  return choose(
    [randomLogicalop, () => Bool(true), () => Bool(false), randomBinop, randomIf(randomBooleanExp)],
    [6 * 2, 1, 1, 5 * 2, blockCount > 8 ? 0.001 : 2.5]
  )
}
function randomLogicalop(): Exp {
  return Prim(choose([() => "and", () => "or"]), randomBooleanExp(), randomBooleanExp())
}
function randomBinop() {
  return Prim(choose(ops), randomIntegerExp(), randomIntegerExp())
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
