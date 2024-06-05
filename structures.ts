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
    for (const [v, w] of this.g) {
      g.push([w, v])
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
    if (this.g.length) throw new Error("cycle detected: " + JSON.stringify(this.g))
    return sorted
  }
  delete(v: T, w: T) {
    let i = 0
    for (const [n, m] of this.g) {
      if (n === v && m === w) {
        this.g.splice(i, 1)
        break
      }
      i++
    }
  }
  orphans(): T[] {
    return this.g.map(([n, _]) => n).filter(n => this.isOrphan(n))
  }
  /** linear, successful is worst case */
  isOrphan(v: T): boolean {
    return !this.g.find(([_, m]) => v === m)
  }
  neighbours(v: T): T[] {
    return this.g.filter(([n, _]) => v === n).map(([_, m]) => m)
  }
}
export type AList<K, V> = { key: K; value: V; next?: AList<K, V> }
export function alist<K, V>(key: K, value: V, next?: AList<K, V>): AList<K, V> {
  return { key, value, next }
}
export function assoc<K, V>(alist: AList<K, V> | undefined, key: K): V | undefined {
  while (alist) {
    if (alist.key === key) return alist.value
    else alist = alist.next
  }
  return undefined
}
export function alistFromMap<K, V, V2>(m: Map<K, V>, f: (v: V) => V2): AList<K, V2> | undefined {
  let head: AList<K, V2> | undefined = undefined
  for (const [k, v] of m) {
    head = alist(k, f(v), head)
  }
  return head
}
