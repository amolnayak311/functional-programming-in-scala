package chapter3

object Exercise3_6 {
  
  /**
   * 
   */
  def init[A](l: List[A]) : List[A] = l match {
      case Nil => throw new UnsupportedOperationException("init on empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, tail) => Cons(x, init(tail))
  }
  
  
  def main(args: Array[String]): Unit = {
    assert(init(List(1, 2, 3, 4)) == List(1, 2, 3))
    assert(init(List(1)) == Nil)
    try {
      init(Nil)
    } catch {
      case e: UnsupportedOperationException =>
        println("All tests successful")
        return
      case _: Throwable =>
    }
    assert(false)    
  }  
}