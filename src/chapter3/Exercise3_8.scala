package chapter3

import chapter3.List._

object Exercise3_8 {
  
  def main(args: Array[String]): Unit = {
    
    // Gives the same List as the Input List
    val frList = foldRight(List(1, 2, 3), List.empty[Int])(Cons(_, _))
    println("frList is " + frList)
    println("equality test on input list and result is " + (List(1, 2, 3) == frList))
  }
}