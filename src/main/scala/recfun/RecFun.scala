package recfun

object RecFun {
  def main(args: Array[String]) {

    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Parentheses Balancing")
    println(balance("())(".toList))

    println("Counting Change")
    println(countChange(300, List(500,5,50,100,20,200,10)))
  }

  /**
   * Pascal's Triangle
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0) 1
      else if (c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Parentheses Balancing
   */
    def balance(chars: List[Char]): Boolean = {
      def paranthesesCheck(chars: List[Char], n: Int): Boolean = {
        if (chars.isEmpty && n == 0) true
        else if (chars.isEmpty && n != 0) false
        else if (chars.head == ')' && n == 0) false
        else {
          val m = if (chars.head == '(') n + 1 else if (chars.head == ')') n - 1 else n
          paranthesesCheck(chars.tail, m)
        }
      }
      paranthesesCheck(chars, 0)
    }
  
  /**
   * Counting Change
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      var cnt = 0
      def loop(sum: Int, coins: List[Int]): Int = {
        if(coins.isEmpty) cnt
        else {
          for (m <- 1 to money / coins.head) {
            cnt = if (sum + (coins.head * m) == money) cnt + 1
            else loop(sum + (coins.head * m), coins.tail)
          }
          loop(sum, coins.tail)
        }
      }
      loop(0, coins)
    }
  }
