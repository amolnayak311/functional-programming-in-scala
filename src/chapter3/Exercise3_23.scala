package chapter3

object Exercise3_23 {
  
  /**
   * 
   */
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) | (Nil, _) => Nil
    case (Cons(h1, tail1), Cons(h2, tail2)) => Cons(f(h1, h2), zipWith(tail1, tail2)(f))
  }
    
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    assert(zipWith(List(1, 2, 3, 4), List(5, 6, 7, 8))(_ + _) == List(6, 8, 10, 12))
    assert(zipWith(List(1, 2, 3, 4), List(5, 6, 7, 8))(_ * _) == List(5, 12, 21, 32))
    println("All tests successful")
  }
  
}