package chapter3

object Exercise3_18 {
  
  /**
   * 
   */
  def map[A, B](l : List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, tail) => Cons(f(x), map(tail)(f))
  }
  
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4)
    assert(map(l)( _ + 1) == List(2, 3, 4, 5))
    assert(map(l)(_.toString) == List("1", "2", "3", "4"))
    assert(map(l)(math.pow(_, 2).toInt) == List(1, 4, 9, 16))
    println("All tests successful")
      
  }
}