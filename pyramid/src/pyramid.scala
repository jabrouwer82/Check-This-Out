package jacob.pyramid

import cats._
import cats.implicits._

/** A pyramid is an upside-down, "perfect" binary tree with overlapping nodes.
 *
 *  Each node in a pyramid is referenced by two nodes in the layers both
 *  above and below it, unless it is on the edge, in which case it's only
 *  referenced by one above and two below.
 *
 *  Because of the strict shape and structure, pyramids are constructed from
 *  a list of rows of nodes (with the base/longest row first) rather than as
 *  a recursive linked structure as is normal for trees.
 *
 *  For example:
 *      a
 *     b c
 *    d e f
 *
 *  Is defined by the Pyramid as:
 *    List(List('d', 'e', 'f'), List('b', 'c'), List('a'))
 */
case class Pyramid[+A](rows: List[List[A]]) {
  val height: Int = rows.length
  val width: Int = height
  def apply(row: Int, col: Int): A = rows(row)(col)
  val top: A = rows.last.head
}

object Pyramid {
  /** Constructs a pyramid that is well-formed, or throws an exception.
   *
   *  These exceptions could be nicer.
   */
  def apply[A](rows: List[List[A]]): Pyramid[A] = {
    // The rows should be a half-square, the width is the same as the height.
    assert(rows.length == 0 || rows.length == rows(0).length)
    val sortedRows = rows.sortBy(_.length * -1)
    // Each row should be one shorter than the previous row.
    assert(sortedRows.map(_.length) == List.range(rows.length, 0, -1))
    Pyramid(sortedRows)
  }

  /** Cats Show implementation that produces a well-shaped but dense pyramid,
   *  formatted narrow to wide with enough space for each node.
   *
   *  For example:
   *  """
   *      58
   *     4  90
   *    1  2  1234
   *  12 3   9    83
   *  """
   */
  implicit def show[A]: Show[Pyramid[A]] = (p: Pyramid[A]) => {
    val columns = p
      .rows
      .mapWithIndex((row, i) =>
        List.fill(i)("") ++ (row.head.toString :: row.tail.flatMap(cell => List("", cell.toString))) ++ List.fill(i)("")
      )
      .transpose
    val maxes = columns.map(_.map(_.length).max)
    columns.zip(maxes)
      .map { case (column, max) =>
        column.map(cell => " " * (max - cell.length) + cell)
      }
      .transpose
      .reverse
      .map(_.mkString)
      .mkString("\n")
  }
}

/** A PyramidSum is a pyramid defined by its base. Each node in upper layers
 *  are computed by summing their two children.
 */
object PyramidSum {
  def apply[A : Semigroup](base: A*): Pyramid[A] =
    PyramidSum.fromBase(base.toList)

  def fromBase[A : Semigroup](base: List[A]): Pyramid[A] =
    Pyramid(
      base :: List.unfold(base)(r => PyramidSum.nextRow(r) match {
        case Nil => None
        case nr => Some(nr -> nr)
      })
    )

  def nextRow[A : Semigroup](row: List[A]): List[A] =
    if (row.length <= 1) List.empty[A]
    else row
      .sliding(2, 1)
      .map { case List(a, b) => a |+| b }
      .toList
}

object PyramidSums extends App {
  // This finds a solution to the following partially filled PyramidSum:
  // """
  //      93
  //     ?  ?
  //    ?  ? 16
  //  11 ?  ?  ?
  // ?  ?  9 ?  ?
  // """
  List.range(1, 10) // The 11 and 16 show that 9 is the largest number on the base.
    .combinations(5) // We only need to select 5 elements to form the base.
    .filter(_.contains(9)) // We know there's a 9 on the base.
    .flatMap(_.permutations) // We want all permutations of each combination.
    .filter(_(2) == 9) // Now we only want ones where the third number is 9.
    .map(PyramidSum.fromBase(_)) // Construct the Pyramids.
    .filter(p => p(1, 0) == 11 && p(2, 2) == 16 && p.top == 93) // Check the known numbers.
    .foreach(p => println(p.show)) // Print 'em.

  // The solution is:
  // """
  //        93
  //      51  42
  //    25  26  16
  //  11  14  12  4
  // 6   5   9   3 1
  // """
}
