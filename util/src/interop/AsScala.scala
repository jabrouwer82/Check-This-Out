package jacob.util.interop

import java.{lang => jl, util => ju}
import scala.jdk.CollectionConverters._

/** Deep conversions from java types to scala types.
 *
 *  Since scala only applies one implicit conversion at a time, this library
 *  enables conversions to immutable collections and the underlying data.
 *
 *  For example:
 *    def javaMethod(): java.util.List[Integer]
 *    val scalaSeq: Seq[Int] = AsScala(javaMethod())
 */
trait AsScala[A, B] {
  def apply(a: A): B
}

trait LowPriorityAsScala {
  implicit def id[A]: AsScala[A, A] = (a: A) => a
}

object AsScala extends LowPriorityAsScala {
  def apply[A, B](a: A)(implicit a2b: AsScala[A, B]): B = a2b(a)

  implicit val void2unit: AsScala[Void, Unit] = _ => ()

  implicit val bool2bool: AsScala[jl.Boolean, Boolean] = Boolean.unbox(_)

  implicit val integer2int: AsScala[Integer, Int] = Int.unbox(_)

  implicit def map2map[K, V, VV](implicit a2b: AsScala[V, VV]): AsScala[ju.Map[K, V], Map[K, VV]] =
    Map[K, VV]() ++ _.asScala.view.mapValues(a2b(_))

  implicit def list2seq[A, B](implicit a2b: AsScala[A, B]): AsScala[ju.List[A], Seq[B]] =
    Seq[B]() ++ _.asScala.map(a2b(_))

  implicit def set2set[A, B](implicit a2b: AsScala[A, B]): AsScala[ju.Set[A], Set[B]] =
    Set[B]() ++ _.asScala.map(a2b(_))
}
