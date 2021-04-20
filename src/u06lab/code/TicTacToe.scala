package u06lab.code

import u06lab.code.TicTacToe.{Mark, O, X, computeAnyGame, find, placeAnyMark, printBoards}


object TicTacToe {
  sealed trait Player {
    def other: Player = this match {
      case X => O;
      case _ => X
    }

    override def toString: String = this match {
      case X => "X";
      case _ => "O"
    }
  }

  case object X extends Player

  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)

  /**
   * A board is a list which contains all the mark
   * If the list is empty, there's no mark
   */
  type Board = List[Mark]
  type Game = List[Board]

  /**
   * return which player (if there is one) that has mark
   * in the board and in the position x,y passed
   */
  def find(board: Board, x: Int, y: Int): Option[Player] = {
    val players = for (mark <- board; if (mark.x == x) && (mark.y == y)) yield mark.player
    if (players.size > 1) throw new UnsupportedOperationException

    /** there are two mark in the same cell */
    players.headOption
  }

  def win(board: Board): Boolean = {
    def winOnColumn(board: Board): Boolean = {
      for (col <- 0 to 2) {
        (find(board, 0, col), find(board, 1, col), find(board, 2, col)) match {
          case (Some(X), Some(X), Some(X)) => return true
          case (Some(O), Some(O), Some(O)) => return true
          case _ =>
        }
      }
      false
    }

    def winOnRow(board: Board): Boolean = {
      for (row <- 0 to 2) {
        (find(board, row, 0), find(board, row, 1), find(board, row, 2)) match {
          case (Some(X), Some(X), Some(X)) => return true
          case (Some(O), Some(O), Some(O)) => return true
          case _ =>
        }
      }
      false
    }

    def winOnDiagonal(board: Board): Boolean = {
      (find(board, 0, 0), find(board, 1, 1), find(board, 2, 2)) match {
        case (Some(X), Some(X), Some(X)) => return true
        case (Some(O), Some(O), Some(O)) => return true
        case _ =>
      }
      (find(board, 2, 0), find(board, 1, 1), find(board, 0, 2)) match {
        case (Some(X), Some(X), Some(X)) => return true
        case (Some(O), Some(O), Some(O)) => return true
        case _ =>
      }
      false
    }

    winOnColumn(board) || winOnRow(board) || winOnDiagonal(board)
  }

  /**
   * Return a sequence of board, all the possibilities for a mark of a player?
   */
  def placeAnyMark(board: Board, player: Player): Seq[Board] =
    for (x <- 0 to 2; y <- 0 to 2; if find(board, x, y).isEmpty && !win(board)) yield Mark(x, y, player) :: board

  /**
   * Compute all possible game with max n moves
   */
  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match {
    case 0 => LazyList(List(Nil))
    case _ => for {
      games <- computeAnyGame(player.other, moves - 1);
      game <- placeAnyMark(games.head, player)
    } yield game :: games
  }

  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) {
        print(" "); if (board == game.head) println()
      }
    }
}

object main extends App {
  // Exercise 1: implement find such that..
  println(find(List(Mark(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Mark(0, 0, X), Mark(0, 1, O), Mark(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Mark(0, 0, X), Mark(0, 1, O), Mark(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement placeAnyMark such that..
  printBoards(placeAnyMark(List(), X))
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...
  printBoards(placeAnyMark(List(Mark(0, 0, O)), X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  var i = 0;
  computeAnyGame(X, 7) foreach { g => printBoards(g); println(); i = i + 1; println(i) }
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
}

