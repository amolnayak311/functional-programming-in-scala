package chapter5

import chapter5.Unfold._
import chapter5.Stream.cons

object Exercise5_16 {
  
  
  /**
   * Use fold right and preserve intermediate results
   */
  def scanRight[A, B](str: Stream[A], z: => B)(f : (A, => B) => B): Stream[B] = 
    str.foldRight(z, Stream(z)){
    case (elem, (zC, str)) => {
      lazy val c = zC
      val fEval = f(elem, c)
      (fEval, cons(fEval, str))
    }
  }._2
  
  def main(args: Array[String]): Unit = {    
    assert(scanRight(Stream(1, 2, 3), 0){_ + _}.toList == List(6, 5, 3, 0))
    println("All tests successful")
  }
}