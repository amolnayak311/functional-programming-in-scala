package chapter5

import chapter5.Stream._

object Exercise5_8 {
  
  /**
   * 
   */
  def constant[A](a:A): Stream[A] = cons( a, constant(a))
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    assert(constant(1).take(3).toList == List(1, 1, 1))
    assert(constant(1).take(3).map(_ + 1).toList == List(2, 2, 2))
    assert(constant('a').take(5).toList ==  List('a', 'a', 'a', 'a', 'a'))
    println("All tests successful")
  }
}