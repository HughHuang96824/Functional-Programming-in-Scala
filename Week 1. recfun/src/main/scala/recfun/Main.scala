package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    var str1 = "((df_)fd)"
    var str2 = "())("
    var str3 = "(()()((0)))"
    println(balance(str1.toList))
    println(balance(str2.toList))
    println(balance(str3.toList))

    var coins = List(1,2)
    var money = 4
    println(countChange(money, coins))
  }


  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def f1(unclosed: Int, string: List[Char]): Boolean = {
      if (unclosed < 0) false
      else if (string.isEmpty) {
        if (unclosed == 0) true
        else false
      }
      else {
        if (string.head == '(') f1(unclosed + 1, string.tail)
        else if (string.head == ')') f1(unclosed - 1, string.tail)
        else f1(unclosed, string.tail)
      }
    }
    f1(0, chars)
  }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0) 0
      else if (coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }

}
