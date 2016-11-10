package chapter3

import chapter3.Exercise3_10.foldLeft

object Exercise3_12 {
  
  
  /**
   * 
   */
  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, List.empty[A])((x, y) => Cons(y, x))
  
  
  def main(args: Array[String]): Unit = {
    assert(reverse(List(1, 2, 3, 4)) == List(4, 3, 2, 1))
    assert(reverse(Nil) == Nil)
    println("All tests successful")
  }
    
}