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
  /**
   * Find vertices that have no incoming edges, recursively, and remove them.
   * @returns vertices that have no incoming edges, recursively
   */
  findDead(start: T): T[] {
    const dead = []
    let deadlen = -1
    while (deadlen < dead.length) {
      deadlen = dead.length
      const vs = this.vertices()
      vs.delete(start)
      for (const live of new Set(this.g.map(([_, w]) => w))) {
        vs.delete(live)
      }
      for (const v of vs) {
        dead.push(v)
        this.g = this.g.filter(([v2, _]) => v2 !== v)
      }
    }
    return dead
  }
  g: [T, T][] = []
  addEdge(v: T, w: T) {
    this.g.push([v, w])
  }
  vertices() {
    const vs: Set<T> = new Set()
    for (const [v, w] of this.g) {
      vs.add(v)
      vs.add(w)
    }
    return vs
  }
  transpose(): [T, T][] {
    const g: [T, T][] = []
    for (const [v, w] of this.g) {
      g.push([w, v])
    }
    return g
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
export function alength(env: AList<any, any> | undefined): number {
  let n = 0
  while (env) {
    n++
    env = env.next!
  }
  return n
}
