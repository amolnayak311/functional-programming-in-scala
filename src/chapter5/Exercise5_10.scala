package chapter5

import chapter5.Stream._

object Exercise5_10 {
  
  /**
   * 
   */
  def fibs: Stream[Int] = {
    def fibRec(n0: Int, n1:Int): Stream[Int] = {
      val n2 = n0 + n1
      cons(n2, fibRec(n1, n2))
    }
    cons(0, cons(1, fibRec(0, 1)))
  }
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    assert(fibs.take(5).toList == List(0, 1, 1, 2, 3))
    assert(fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
    assert(fibs.take(0).toList == Nil)
    println("All tests successful")
  }
}