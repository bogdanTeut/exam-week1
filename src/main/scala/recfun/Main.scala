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
    def pascal(c: Int, r: Int): Int =
      if (c==0 | r==c) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def getCounter(counter: Int, head: Char) = {
        if (head == '(') counter + 1
        else if (head == ')') counter - 1
        else counter
      }

      def balanceIter(remainingChars: List[Char], counter: Int): Boolean = {
        if (remainingChars.isEmpty) counter == 0
        else if (counter < 0) false
        else {
          val newCounter =  getCounter(counter, remainingChars.head)
          balanceIter(remainingChars.tail, newCounter)
        }
      }

      balanceIter(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countSortedChange(money: Int, coins: List[Int]): Int = {
        def countIter(money: Int, coins: List[Int]): Int = {
          if (coins.size == 0 || money < coins.head) 0
          else if (money == coins.head) 1
          else countChange(money-coins.head, coins)
        }

        def countTailCoinsChange = {
          if (coins.size > 1 && (money >= coins.tail.head))
            countSortedChange(money, coins.tail)
          else 0
        }

        if (coins.size == 0) 0
        else countIter(money, coins) + countTailCoinsChange
      }

      countSortedChange(money, coins.sorted)
    }
  }
