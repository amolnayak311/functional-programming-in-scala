package chapter3

import chapter3.List._

object Exercise3_9 {
  
  def length[A](a: List[A]): Int = foldRight(a, 0)((_, a) => a + 1)
  
  def main(args: Array[String]): Unit = {
      assert(length(List.empty[Int]) == 0)
      assert(length(List(1, 2, 3, 4)) == 4)
      println("All tests successful")
  }
}