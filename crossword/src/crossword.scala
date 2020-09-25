package jacob.crossword

import cats._
import cats.implicits._

trait Direction
case object Down extends Direction
case object Across extends Direction

case class Word(value: String, clue: String = "") {
}

case class Crossword(words: Map[(Direction, Int, Int), Word]) {
  val width: Int = words.foldLeft(0)((acc, cur) => cur match {
    case ((Down, wx, _), _) => math.max(acc, wx + 1)
    case ((Across, wx, _), w) => math.max(acc, wx + w.value.length)
  })
  val height: Int = words.foldLeft(0)((acc, cur) => cur match {
    case ((Across, _, wy), _) => math.max(acc, wy + 1)
    case ((Down, _, wy), w) => math.max(acc, wy + w.value.length)
  })
  val grid: Map[(Int, Int), Char] =
    words.flatMap(_ match {
      case ((Down, wx, wy), w) => w.value.toList.mapWithIndex((char, i) => (wx, wy + i) -> char)
      case ((Across, wx, wy), w) => w.value.toList.mapWithIndex((char, i) => (wx + i, wy) -> char)
    })
  val rows: List[List[(Int, Int, Option[Char])]] =
    List.tabulate(height)(y =>
      List.tabulate(width)(x =>
        (x, y, charAt(x, y))
      )
    )
  val board: List[List[Boolean]] =
    rows.map(_.map(_._3.nonEmpty))


  val fillRatio: Double = rows.flatten.flatMap(_._3).length.toDouble / width / height
  val symmetry: Double = board

  // Lower is better.
  val score: Double = 1 / fillRatio

  def charAt(x: Int, y: Int): Option[Char] = grid.get(x -> y)

  def withWord(word: Word): Option[Crossword] =
    (withWordDown(word) ++ withWordAcross(word)).sorted.headOption

  def withWordDown(word: Word): List[Crossword] =
    word.value.toList
      .zipWithIndex
      .foldLeft(possibleStartsDown(word)) { case (acc, (cur, i)) =>
        acc.filter { case (x, y) =>
          (charAt(x, y+i), charAt(x-1, y+i), charAt(x+1, y+i))  match {
            case (Some(c), _, _) => c == cur
            case (None, None, None) => true
            case (None, _, _) => false
          }
        }
      }
      .map { case (x, y) => Crossword(words + ((Down, x, y) -> word)) }

  def withWordAcross(word: Word): List[Crossword] =
    word.value.toList
      .zipWithIndex
      .foldLeft(possibleStartsAcross(word)) { case (acc, (cur, i)) =>
        acc.filter { case (x, y) =>
          (charAt(x+i, y), charAt(x+i, y-1), charAt(x+i, y+1))  match {
            case (Some(c), _, _) => c == cur
            case (None, None, None) => true
            case (None, _, _) => false
          }
        }
      }
      .map { case (x, y) => Crossword(words + ((Across, x, y) -> word)) }

  def possibleStartsAcross(word: Word): List[(Int, Int)] =
    rows.flatten
      .collect(_ match {
        case (x, y, None) if charAt(x-1, y) == None => (x, y)
        case (x, y, Some(c)) if c == word.value(0) && charAt(x-1, y) == None => (x, y)
      })

  def possibleStartsDown(word: Word): List[(Int, Int)] =
    rows.flatten
      .collect(_ match {
        case (x, y, None) if charAt(x, y-1) == None => (x, y)
        case (x, y, Some(c)) if c == word.value(0) && charAt(x, y-1) == None => (x, y)
      })
}

object Crossword {
  implicit val show: Show[Crossword] = (cw: Crossword) =>
    List.tabulate(cw.height)(y =>
      List.tabulate(cw.width)(x =>
        cw.charAt(x, y).getOrElse('█')
      ).mkString("║", "│", "║")
    ).mkString(
      List.tabulate(cw.width * 2 - 1)(i => if (i % 2 == 0) '═' else '╤').mkString("\n╔", "", "╗\n"),
      List.tabulate(cw.width * 2 - 1)(i => if (i % 2 == 0) '─' else '┼').mkString("\n╟", "", "╢\n"),
      List.tabulate(cw.width * 2 - 1)(i => if (i % 2 == 0) '═' else '╧').mkString("\n╚", "", s"╝\nscore: ${cw.score}\n"),
    )

  implicit val ord: Ordering[Crossword] = Ordering.by(_.score)
}

object Main extends App {
  val words = List(
    "golden",
    "at",
    "sour",
    "ie",
    "so",
    "

    "gate",
    "ot",
  )
  val cw = Crossword(Map(
    (Down, 0, 0) -> Word("jacob"),
    (Across, 0, 0) -> Word("john"),
  ))
  println(cw.show)
  val cw2 = for {
    one <- cw.withWord(Word("howl"))
    two <- one.withWord(Word("cowl"))
  } yield two
  println(cw2.show)
}

// case class Crossword(words: Map[(Int, Int), String], height: Int, width: Int) {
//   def grid: CrosswordGrid = ???
//   // def score: Double = grid.count(_ != ' ').toDouble / grid.length
// }

// case class CrosswordGrid(grid: List[List[Char]]) {
//   val linearGrid: List[Char] = grid.flatten
//   val height: Int = grid.length
//   val width = grid.headOption.fold(0)(_.length)

//   def |+|(other: CrosswordGrid): CrosswordGrid =
//     CrosswordGrid(
//       linearGrid.zip(other.linearGrid)
//         .map(_ match {
//           case (' ', ' ') => ' '
//           case (' ', x) => x
//           case (x, _) => x
//         })
//         .sliding(width, width)
//         .toList
//   )
// }

// object Crossword {
//   def apply(grid: List[String]): Crossword = {
//     val maxLen = grid.map(_.length).max
//     Crossword(grid.flatMap(_.padTo(maxLen, ' ')), maxLen)
//   }

//   implicit val show: Show[Crossword] = (cw: Crossword) =>
//     cw.grid
//       .map(letter => if (letter == ' ') ' ' else '▇')
//       .sliding(cw.width, cw.width)
//       .map(_.mkString)
//       .mkString("\n")
// }

// object Generation extends App {
//   val words = List(
//     "qwer",
//     "q  x",
//     "x  b",
//     "bgtr",
//   )
//   val cw = Crossword(words)
//   println(cw.show)
//   println(cw.score)
//   val words2 = List(
//     "q",
//     "qwer",
//     "x",
//     "bgtr",
//     "   x",
//     "   b",
//     "   r",
//   )
//   val cw2 = Crossword(words2)
//   println(cw2.show)
//   println(cw2.score)
// }
