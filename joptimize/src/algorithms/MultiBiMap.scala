package joptimize.algorithms

import scala.collection.mutable

/**
  * A map from keys to collections of values: you can assign multiple values
  * to any particular key. Also allows lookups in both directions: what values
  * are assigned to a key or what key a value is assigned to.
  */
trait MultiBiMap[K, V]{
  def containsValue(v: V): Boolean
  def lookupKey(k: K): Seq[V]
  def lookupValue(v: V): K
  def lookupValueOpt(v: V): Option[K]
  def add(k: K, v: V): Unit
  def removeAll(k: K): Seq[V]
  def addAll(k: K, vs: TraversableOnce[V]): Unit
  def keys(): Iterator[K]
  def items(): Iterator[(K, Seq[V])]
  def values(): Iterator[Seq[V]]
  def keyCount: Int
}

object MultiBiMap{

  class Mutable[K, V]() extends MultiBiMap[K, V]{
    private[this] val valueToKey = mutable.LinkedHashMap.empty[V, K]
    private[this] val keyToValues = mutable.LinkedHashMap.empty[K, mutable.Buffer[V]]
    def containsValue(v: V) = valueToKey.contains(v)
    def lookupKey(k: K) = keyToValues(k)
    def lookupKeyOpt(k: K) = keyToValues.get(k)
    def lookupValue(v: V) = valueToKey(v)
    def lookupValueOpt(v: V) = valueToKey.get(v)
    def add(k: K, v: V): Unit = {
      valueToKey(v) = k
      keyToValues.getOrElseUpdate(k, new mutable.ArrayBuffer[V]()).append(v)
    }
    def removeAll(k: K): Seq[V] = keyToValues.get(k) match {
      case None => Seq()
      case Some(vs) =>
        vs.foreach(valueToKey.remove)

        keyToValues.remove(k)
        vs
    }
    def addAll(k: K, vs: TraversableOnce[V]): Unit = vs.foreach(this.add(k, _))

    def keys() = keyToValues.keysIterator

    def values() = keyToValues.valuesIterator

    def items() = keyToValues.iterator

    def keyCount = keyToValues.size
  }
}