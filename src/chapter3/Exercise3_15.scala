package chapter3

import List.foldRight

object Exercise3_15 {
  
  
  /**
   * 
   * 
   */
  // Not a tail recursive implementation
  def flatten[A](l : List[List[A]]): List[A] = {
      foldRight(l, List.empty[A]) {
        (list, accumulated) => {
          foldRight(list, accumulated) {(elem, acc) => Cons(elem, acc)}
        }
      }
  }
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    val l1 = List(1, 2, 3, 4)
    val l2 = List(5, 6, 7)
    assert(flatten(List(l1, l2)) == List(1, 2, 3, 4, 5, 6, 7))
    println("All tests successful")
  }
}