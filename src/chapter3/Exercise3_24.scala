package chapter3

object Exercise3_24 {
  
  
  /**
   * 
   */
  def beginsWith[A](large: List[A], small: List[A]): Boolean = (large, small) match {
      case (Nil, Nil) => true
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, tail1), Cons(h2, tail2)) => 
                if(h1 == h2) beginsWith(tail1, tail2) else false
  }
  
  /**
   * Not super efficient as any String search Algorithm for example
   */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, Nil) => true
    case (Nil, _) => false
    case (_, _) if(beginsWith(sup, sub)) => true
    case (Cons(_, tail), _) => hasSubsequence(tail, sub)
  }
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)) == true)
    assert(hasSubsequence(List(1, 2, 3, 4), List(6, 7)) == false)
    assert(hasSubsequence(List(1, 2, 3, 4), Nil) == true)
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) == true)
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4, 5)) == false)
    assert(hasSubsequence(Nil, List(1, 2, 3, 4)) == false)
    assert(hasSubsequence(Nil, Nil) == true)
    println("All tests successful")
  }
}