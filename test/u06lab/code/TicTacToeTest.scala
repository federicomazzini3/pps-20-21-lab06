package u06lab.code

import org.junit.jupiter.api.Assertions.{assertFalse, assertTrue}
import org.junit.jupiter.api.Test
import u06lab.code.TicTacToe.{Board, Mark, O, X, win}

class TicTacToeTest {
  @Test
  def testWinOnRow(): Unit ={
    val winBoard: Board = List(Mark(0, 0, X), Mark(0, 1, X), Mark(0, 2, X), Mark(1,2,O), Mark(2,2,O))
    val notWinBoard: Board = List(Mark(2, 0, X), Mark(0, 1, X), Mark(0, 2, X), Mark(1,2,O), Mark(2,2,O))
    assertTrue(win(winBoard))
    assertFalse(win(notWinBoard))
  }

  @Test
  def testWinOnColumn(): Unit ={
    val winBoard: Board = List(Mark(0, 0, X), Mark(1, 0, X), Mark(2, 0, X), Mark(1,2,O), Mark(2,2,O))
    val notWinBoard: Board = List(Mark(0, 0, O), Mark(1, 0, O), Mark(2, 0, X), Mark(1,2,O), Mark(2,2,O))
    assertTrue(win(winBoard))
    assertFalse(win(notWinBoard))
  }
  @Test
  def testWinOnDiagonal(): Unit ={
    val winBoard: Board = List(Mark(0, 0, O), Mark(1, 1, O), Mark(2, 2, O), Mark(1,2,O), Mark(0,2,O))
    val notWinBoard: Board = List(Mark(2, 0, X), Mark(1, 1, O), Mark(2, 2, X), Mark(1,2,O), Mark(0,2,O))
    assertTrue(win(winBoard))
    assertFalse(win(notWinBoard))
  }

  @Test
  def testWinOnAntiDiagonal(): Unit ={
    val winBoard: Board = List(Mark(0, 2, X), Mark(1, 1, X), Mark(2, 0, X), Mark(1,2,O), Mark(2,2,O))
    val notWinBoard: Board = List(Mark(0, 2, O), Mark(1, 1, X), Mark(2, 0, X), Mark(1,2,O), Mark(2,2,O))
    assertTrue(win(winBoard))
  }

}
