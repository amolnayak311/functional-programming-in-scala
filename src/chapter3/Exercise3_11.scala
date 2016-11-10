package chapter3

import chapter3.Exercise3_10.foldLeft

object Exercise3_11 {
  
    /**
     * 
     */
    def product(xs: List[Int]): Int = foldLeft(xs, 1)(_ * _)
    
    
    /**
     * 
     */
    def sum(xs: List[Int]) : Int = foldLeft(xs, 0)(_ + _)
    
    /**
     * 
     */
    def length[A](a: List[A]): Int = foldLeft(a, 0)((a, _) => a + 1)
    
    
    def main(args: Array[String]): Unit = {
      val l = List(1, 2, 3, 4)
      assert(product(l) == 24)
      assert(sum(l) == 10)
      assert(length(l) == 4)
      assert(product(Nil) == 1)
      assert(sum(Nil) == 0)
      assert(length(Nil) == 0)
      println("All Tests successful")
          
    }
    
}