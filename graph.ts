import assert from "node:assert"
import { assertDefined } from "./core.js"
export class DirectedGraph<T> {
  g: Map<T, Set<T>> = new Map()
  addVertex(v: T) {
    assert(!this.g.has(v))
    this.g.set(v, new Set())
  }
  getAndOrAddVertex(v: T) {
    if (this.g.has(v)) return this.g.get(v)!
    const a: Set<T> = new Set()
    this.g.set(v, a)
    return a
  }
  addEdge(v: T, w: T) {
    this.getAndOrAddVertex(v).add(w)
    this.getAndOrAddVertex(w).add(v)
  }
  inNeighbours(v: T): Set<T> {
    return assertDefined(this.g.get(v))
  }
  inVertices(): T[] {
    return Array.from(this.g.keys())
  }
}
