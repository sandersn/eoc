import assert from "node:assert"
export function assertDefined<T>(e: T | undefined): T {
  assert(e !== undefined)
  return e
}

/** Read a number from stdin
 * use readline something something can't be bothered
 */
export function read() {
  return 0
}