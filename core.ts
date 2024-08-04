import assert from "node:assert"
export function assertDefined<T>(e: T | undefined): T {
  assert(e !== undefined)
  return e
}

export function unzip<T, U>(arr: Array<[T, U]>): [T[], U[]] {
  const ts = []
  const us = []
  for (const [t, u] of arr) {
    ts.push(t)
    us.push(u)
  }
  return [ts, us]
}
export function zip<T, U>(ts: T[], us: U[]): Array<[T, U]> {
  const tus = []
  assert(ts.length === us.length)
  for (let i = 0; i < ts.length; i++) {
    tus.push([ts[i], us[i]] as [T, U])
  }
  return tus
}
export type Box = { ref: Value }
export function box(value: Value): Box {
  return { ref: value }
}
export function unbox(box: Box): Value {
  return box.ref
}
export function setBox(box: Box, value: Value): void {
  box.ref = value
}
/** Read a number from stdin
 * use readline something something can't be bothered
 * TODO: Better for testing: use a counter or a random number generator with a seed
 */
export function read() {
  return 0
}
let counter = 0
export function gensym() {
  return "g" + counter++
}
export type Value = { kind: 'int', value: number } | { kind: 'vector', values: Value[] } | { kind: 'void' }
export type Type = symbol | { kind: "vector"; types: Type[] }
export const intType = Symbol("Integer")
export const boolType = Symbol("Boolean")
export const voidType = Symbol("Void")