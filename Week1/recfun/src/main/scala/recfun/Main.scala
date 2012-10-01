package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def okIfEven(opens: Int, closed: Int, chars: List[Char]): Boolean = {
      if (closed > opens) false
      else if (chars.isEmpty) opens == closed
      else {
        okIfEven(
          opens + (if (chars.head == '(') 1 else 0),
          closed + (if (chars.head == ')') 1 else 0),
          chars.tail)
      }
    }

    okIfEven(0, 0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0) 0
      else if (money == 0) 1
      else if (coins.length == 0) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
