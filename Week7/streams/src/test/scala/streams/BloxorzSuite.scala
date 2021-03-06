package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._
import java.util.logging.StreamHandler

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos === Pos(1,1))
      assert(goal === Pos(4,7))
    }
  }

  test("Is standing") {
    new Level1 {
      assert(Block(Pos(2, 3), Pos(2, 3)).isStanding == true)
      assert(Block(Pos(2, 3), Pos(3, 3)).isStanding == false)
    }
  }

  test("Is leagal") {
    new Level1 {
      assert(Block(Pos(2, 3), Pos(3, 3)).isLegal == true)
      assert(Block(Pos(2, 0), Pos(3, 0)).isLegal == false)
    }
  }

  test("StartBlock") {
    new Level1 {
      assert(startBlock === Block(Pos(1, 1), Pos(1, 1)))
    }
  }

  test("neighbors standing") {
    new Level1 {
      assert(Block(Pos(3, 3), Pos(3, 3)).neighbors.toSet ===
        Set((Block(Pos(3, 4), Pos(3, 5)), Right),
            (Block(Pos(4, 3), Pos(5, 3)), Down),
            (Block(Pos(3, 1), Pos(3, 2)), Left),
            (Block(Pos(1, 3), Pos(2, 3)), Up)
        ))
    }
  }

  test("neighbors lying") {
    new Level1 {
      assert(Block(Pos(3, 3), Pos(3, 4)).neighbors.toSet ===
        Set((Block(Pos(3, 5), Pos(3, 5)), Right),
          (Block(Pos(4, 3), Pos(4, 4)), Down),
          (Block(Pos(3, 2), Pos(3, 2)), Left),
          (Block(Pos(2, 3), Pos(2, 4)), Up)
        ))
    }
  }

  test("leagal neighbors") {
    new Level1 {
      assert(startBlock.legalNeighbors.toSet ===
        Set((Block(Pos(1, 2), Pos(1, 3)), Right),
          (Block(Pos(2, 1), Pos(3, 1)), Down)
        ))
    }
  }

  test("Done") {
    new Level1 {
      assert(done(startBlock) == false)
      assert(done(Block(Pos(4, 7), Pos(5, 7))) == false)
      assert(done(Block(Pos(4, 7), Pos(4, 7))) == true)
    }
  }

  test("Finding neighbors") {
    new Level1 {
      val expected = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )

      assert(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet === expected)
    }
  }

  test("New Neighbors Only") {
    new Level1 {
      val expected = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      val result = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,

        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )

      assert(result === expected)
    }
  }


  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length === optsolution.length)
    }
  }

  test("Empty stream in from") {
    new Level1 {
      assert(from(Stream.empty, Set()) === Stream.empty)
    }
  }
}
