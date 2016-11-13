package chapter3

object Exercise3_17 {
  
  
  /**
   * 
   */
  def mapToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, tail) => Cons(x.toString, mapToString(tail))
  }
  
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    assert(mapToString(List(1.0, 2.0, 3.1, 4.2)) == List("1.0", "2.0", "3.1", "4.2"))
    println("All test cases successful")
  }
}