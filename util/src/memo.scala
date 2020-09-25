package jacob.util

import scala.collection.mutable.{Map => MMap}

/** Thread-safe memo wrapper for functions. */
class Memo1[A, B](f: A => B) extends (A => B) {
  val cache = MMap.empty[A, B]

  def apply(a: A) = synchronized(cache.getOrElseUpdate(a, f(a)))
}

/** Thread-safe memo wrapper for functions. */
class Memo2[A, B, C](f: Function2[A, B, C]) extends Function2[A, B, C] {
  val cache = MMap.empty[(A, B), C]

  def apply(a: A, b: B) = synchronized(cache.getOrElseUpdate((a, b), f(a, b)))
}

/** Factories to more easily summon Memo implementations. */
object Memo {
  def apply[A, B](f: A => B) = new Memo1(f)
  def apply[A, B, C](f: Function2[A, B, C]) = new Memo2(f)
}

