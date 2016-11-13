package chapter3

object Exercise3_16 {
  
  /**
   * 
   */
  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, tail) => Cons(x + 1, addOne(tail))
  }
  
  
  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4)
    assert(addOne(l) == List(2, 3, 4, 5))
    println("All tests successful")
  }
}