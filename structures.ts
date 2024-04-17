import assert from "node:assert"
import { assertDefined } from "./core.js"
export class Graph<T> {
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
export class DirectedGraph<T> {
  g: [T, T][] = []
  addEdge(v: T, w: T) {
    this.g.push([v, w])
  }
  transpose() {
    const g: [T, T][] = []
    for (const [v,w] of this.g) {
      g.push([w,v])
    }
    this.g = g
  }
  sort(): T[] {
    const sorted = []
    const orphans = this.orphans()
    while (orphans.length) {
      const n = orphans.shift()! // pop would also work fine
      sorted.push(n)
      for (const m of this.neighbours(n)) {
        this.delete(n, m)
        if (this.isOrphan(m)) {
          orphans.push(m)
        }
      }
    }
    if (this.g.length)
      throw new Error("cycle detected: " + JSON.stringify(this.g))
    return sorted
  }
  delete(v: T, w: T) {
    let i = 0
    for (const [n,m] of this.g) {
      if (n === v && m === w) {
        this.g.splice(i, 1)
        break
      }
      i++
    }
  }
  orphans(): T[] {
    return this.g.map(([n,_]) => n).filter(n => this.isOrphan(n))
  }
  /** linear, successful is worst case */
  isOrphan(v: T): boolean {
    return !this.g.find(([_,m]) => v === m)
  }
  neighbours(v: T): T[] {
    return this.g.filter(([n,_]) => v === n).map(([_,m]) => m)
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
