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
/** Read a number from stdin
 * use readline something something can't be bothered
 */
export function read() {
  return 0
}