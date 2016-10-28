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
      if(r == c) 1
      else if(c < 0 || c > r) 0
      else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        def balanceCore(chars: List[Char], lefts: Int): Boolean = {
          if(chars.isEmpty) lefts == 0
          else {
            val balance = chars.head match {
              case '(' => lefts + 1
              case ')' => lefts - 1
              case default => lefts
            }
            if(balance < 0) false
            else balanceCore(chars.tail, balance)
          }
        }
        balanceCore(chars, 0)
      }
      
    def balance_iterative(chars: List[Char]): Boolean =
      chars.foldLeft(0)((z, i) => 
        if(z < 0) z
        else if(i == '(') z + 1
        else if(i == ')') z - 1
        else z
      ) == 0
      
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(coins.isEmpty || money == 0) 0
      else {
        val headCount =
          if(money == coins.head) 1
          else if(money > coins.head) countChange(money - coins.head, coins)
          else 0
        
        headCount + countChange(money, coins.tail)
      }
    }
  }
