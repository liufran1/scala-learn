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
      require(c <= r, "Columns must be less than or equal to number of rows")
      if (c == 0 || r == 0 || r == c) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      //need to define function first, otherwise scala doesn't read the final 'else' as the return,
      //so it will return an AnyVal of Unit, which is equivalent to void
      //Use an accumulator
      def loop(acc: Int, chars: List[Char]): Int = {
        if (chars.isEmpty) 0
        else if (chars.head == '(') loop(acc + 1, chars.tail)
        else if (chars.head == ')')
          if (acc > 0) loop(acc - 1, chars.tail)
          else -1
        else loop(acc, chars.tail)
      }
      if (loop(0, chars) == 0) true
      else false
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      //Try not to loop over all n unique coins, 2^n combinations so exponential running time
      //Don't want to double count
      //Loop over fewest number coins to most
      def loop(acc: Int, money: Int, coins: List[Int]): Int = {
        /*
        println("Current count: ")
        println(acc)
        println("Coins considered:")
        println(coins)
        println("Current money: ")
        println(money)
        */
        if (coins.isEmpty) acc
          //need to fix this DFS
        else if (money - coins.head == 0) loop(acc+1, money + coins.head, coins.tail)
        else if (money - coins.head > 0) loop(acc, money - coins.head, coins)
        else loop(acc, money + coins.head, coins.tail)
      }
      if (coins.isEmpty) 0
      else loop(0, money, coins) + countChange(money, coins.tail)
    }
  }
