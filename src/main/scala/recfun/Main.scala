package recfun

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
    if (c == 0 || r == c || r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    * Write a recursive function which verifies the balancing of parentheses in a string, which we represent as a List[Char] not a String.
    * For example, the function should return true for the following strings:
    * (if (zero? x) max (/ 1 x))
    * I told him (that it’s not (yet) done). (But he wasn’t listening)
    *
    * The function should return false for the following strings:
    * :-)
    * ())(
    */
  def balance(chars: List[Char]): Boolean = {
    def iterate(chars: List[Char], acc: Int): Int = {
      if (chars.isEmpty || acc < 0) acc
      else if (chars.head == '(') iterate(chars.tail, acc + 1)
      else if (chars.head == ')') iterate(chars.tail, acc - 1)
      else iterate(chars.tail, acc)
    }

    iterate(chars, 0) == 0
  }

  /**
    * Exercise 3
    * Write a recursive function that counts how many different ways you can make change for an amount,
    * given a list of coin denominations.
    * For example, there are 3 ways to give change for 4 if you have coins with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else if (money <= 0 && coins.nonEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)

}
