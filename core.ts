import assert from "node:assert"
export function assertDefined<T>(e: T | undefined): T {
  assert(e !== undefined)
  return e
}
