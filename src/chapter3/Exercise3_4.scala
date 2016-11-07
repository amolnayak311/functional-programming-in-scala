package chapter3

//for reusing tail
import chapter3.Exercise3_2.tail

object Exercise3_4 {

  /**
   * 
   */
  def drop[A](n: Int, list:List[A]): List[A] = 
    if(n == 0) list else
    list match {
      case Nil => Nil
      case Cons(_, y) => drop(n - 1, y)
    }
  
  def main(args: Array[String]): Unit = {
    assert(drop(10, List(1, 2, 3)) == Nil)
    assert(drop(10, Nil) == Nil)
    assert(drop(1, List(1, 2, 3)) == List(2, 3))
    assert(drop(3, List(1, 2, 3)) == Nil)
    println("All tests successful")
  }
}