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
/** Association list, like from Lisp */
export class AList<K, V> {
  constructor(
    public key: K,
    public value: V,
    public next?: AList<K, V>,
  ) {}
  get(key: K): V | undefined {
    if (this.key === key) return this.value
    else if (this.next) return this.next.get(key)
  }
  set(key: K, value: V): void {
    if (this.key === key) this.value = value
    else if (this.next) this.next.set(key, value)
    else throw new Error(`Key ${key} not found`)
  }
  toMap(m: Map<K, V>) {
    m.set(this.key, this.value)
    if (this.next) this.next.toMap(m)
    return m
  }
  static fromMap<K, V>(m: Map<K, V>): AList<K, V> {
    let alist: AList<K, V> | undefined = undefined
    for (const [k, v] of m) {
      alist = new AList(k, v, alist)
    }
    assert(alist)
    return alist
  }
}
