package chapter5

import chapter5.Stream._

object Exercise5_9 {
  
  /**
   * 
   */
  def from(n:Int): Stream[Int] = cons(n,  from(n + 1))
  
  def main(args: Array[String]): Unit = {
    assert(from(1).take(5).toList == List(1, 2, 3, 4, 5))
    assert(from(10).take(3).toList == List(10, 11, 12))
    println("All tests successful")
  }
  
}