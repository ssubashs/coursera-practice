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
    if (c <= 0) 1
    else {
      if (c == r) 1
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def isOpeningBrace(x: Char): Boolean = if (x == '(') true else false
    def isClosingBrace(x: Char): Boolean = if (x == ')') true else false

    def checkBalance(opBraces: Int, localChars: List[Char]): Boolean = {
      if (localChars.isEmpty) {
        if (opBraces <= 0) true else false
      } else {
        val head = localChars.head
        if (isClosingBrace(head)) {
          if (opBraces <= 0) false else checkBalance(opBraces - 1, localChars.tail)
        }
        else if (isOpeningBrace(head)) {
          checkBalance(opBraces + 1, localChars.tail)
        }
        else {
          checkBalance(opBraces, localChars.tail)
        }
      }
    }

    def removeAllNonBraces(tempChars: List[Char]): List[Char] = {
      if (tempChars.isEmpty) tempChars;
      val temp = tempChars.head;
      if (isClosingBrace(temp) || isOpeningBrace(temp))
        tempChars
      else
        removeAllNonBraces(tempChars.tail)
    }
    checkBalance(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
   var sum = 0;
   def count(tempMoney:Int, availableCoins:List[Int]): Unit ={
     if(availableCoins.isEmpty) {
       return // Theres no future for this path.
     }

     if(tempMoney == 0){ // we have already found a combination.
       sum = sum+1;
     }else {
       val firstElement = availableCoins.head;
       if (firstElement == tempMoney) {
         sum = sum + 1;
         count(tempMoney, availableCoins.tail); //found one with head, lets go hunt for other combinations leaving that element.
       } else if (firstElement > tempMoney) { // can't find combination for this sum with this element so lets go furthur.
         count(tempMoney, availableCoins.tail);
       } else {
         count(tempMoney - firstElement, availableCoins)
         count(tempMoney, availableCoins.tail)
       }

     }
   }

    count(money,coins)
    return sum // after evaluating count.

  }
}
