package chapter3

object Exercise3_5 {
  
  /**
   * 
   */
  def dropWhile[A](l: List[A], f:  A => Boolean) : List[A] = l match {
    case Nil => Nil
    case Cons(y, tail) =>
      if(f(y))
        dropWhile(tail, f)
        else
          l
  }
  
  def main(args: Array[String]): Unit = {
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x > 5) == List(1, 2, 3 ,4))
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x < 5) == Nil)
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x < 3) == List(3, 4))
    println("All tests successful")
  }
    
}