package chapter3

import chapter3.List._

object Exercise3_14 {
  
  
  /**
   * 
   */
  def append[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))
  
  
  def main(args: Array[String]): Unit = {
    assert(append(Nil, Nil) == Nil)
    assert(append(Nil, List(1, 2, 3, 4)) == List(1, 2, 3, 4))
    assert(append(List(1, 2, 3, 4), Nil) == List(1, 2, 3, 4))
    assert(append(List(1, 2, 3), List(4, 5, 6)) == (List(1, 2, 3, 4, 5, 6)))
    println("All tests successful")
  }
     
}