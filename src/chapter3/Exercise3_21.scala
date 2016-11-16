package chapter3

import chapter3.Exercise3_20.flatMap

object Exercise3_21 {
  
  /**
   * Implement filter using flatMap
   */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = 
    flatMap(l)(a => if(f(a)) List(a) else Nil)
    
    
   def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4)
     assert(filter(l)(_ > 5) == Nil)
     assert(filter(l)(_ < 5) == l)
     assert(filter(l)(_ < 3) == List(1, 2))
     println("All tests successful")
   }
}