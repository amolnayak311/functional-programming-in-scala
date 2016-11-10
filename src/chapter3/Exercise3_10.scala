package chapter3

import scala.annotation.tailrec

object Exercise3_10 {
  
  //Implement foldLeft which is tail recursive
  
  /**
   * 
   */
  @tailrec
  def foldLeft[A, B](l: List[A], b: B)(f : (B, A) => B): B = l match {
    case Nil => b
    case Cons(h, t) =>  foldLeft(t, f(b, h))(f)
  }
  
  
  def main(args: Array[String]): Unit = {
    assert(foldLeft(List(1, 2, 3, 4), 0)(_ - _) == -10)
    assert(foldLeft(List.empty[Int], 0)(_ - _) == 0)
    println("All test cases are successful")
  }
}