package chapter3

object Exercise3_2 {
  
  /**
   * 
   */
  def tail[A](list: List[A]): List[A] = list match {
    case Cons(x, y) => y
    case Nil => throw new UnsupportedOperationException("tail of empty list")
  }
  
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    assert(tail(List(1, 2, 3, 4)) == List( 2, 3, 4))
    try {
      tail(Nil)
    } catch {
      case u:UnsupportedOperationException => 
        assert(true)
        println("All tests successful")
        return
      case _: Throwable => 
    }
    assert(false)
  }
  
}