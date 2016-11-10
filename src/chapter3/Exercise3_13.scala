package chapter3

import chapter3.Exercise3_10.foldLeft
import chapter3.Exercise3_12.reverse
import chapter3.List._

object Exercise3_13 {

    /**
     * 
     */
    def foldRight1[A, B](xs: List[A], b: B)(f: (A, B) => B):B = 
      foldLeft(reverse(xs), b)((x, y) => f(y, x)) 
    
   //Fold left can be implement similarly by reversing the list
      
  //TODO: Come up with an alternate implementation without reversing
      
    /**
     *   
     */
    def main(args: Array[String]): Unit = {
      val l = List(1, 2, 3, 4)
      val f1 = foldRight(l, 0)(_ - _)
      val f2 = foldRight1(l, 0)(_ - _)
      
      assert(f1 == f2)
      assert(foldRight(List.empty[Int], 0)(_ - _) == foldRight1(List.empty[Int], 0)(_ - _))
      println("All tests successful")
    }
      
}