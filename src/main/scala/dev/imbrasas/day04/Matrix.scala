package dev.imbrasas.day04

case class Matrix(matrix: List[List[Either[Int, Int]]]):
  def mark(number: Int): Matrix =
    val matrix_ = matrix.map {
      _.map {
        case Right(n) if n == number => Left(n)
        case x                       => x
      }
    }

    Matrix(matrix_)

  def check: Boolean =
    inline def checkRow(row: List[Either[Int, Int]]) = row.forall(_.isLeft)
    val hasCompleteRow = matrix.find(checkRow(_)).isDefined
    hasCompleteRow || matrix.transpose.find(checkRow(_)).isDefined

  def sumUnmarked: Int =
    matrix.foldLeft(0) { (acc, row) =>
      acc + row.foldLeft(0) {
        case (acc, Right(i)) => acc + i
        case (acc, _)        => acc
      }
    }

  override lazy val toString =
    matrix
      .map { row =>
        row.map {
          case Left(i)  => s"(${i})".padTo(5, ' ')
          case Right(i) => s"${i}".padTo(5, ' ')
        }.mkString
      }
      .mkString("\n")
