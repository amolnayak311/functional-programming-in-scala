package chapter3

import chapter3.Exercise3_18.map
import chapter3.Exercise3_15.flatten

object Exercise3_20 {
  
  // Lets implement flatMap using flatten and map
  
  /**
   * 
   */
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = flatten(map(l)(f))
  
  
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4)
    assert(flatMap(l)(e => List(e, e)) == List(1, 1, 2, 2, 3, 3, 4, 4))
    println("All tests successful")
  }
}