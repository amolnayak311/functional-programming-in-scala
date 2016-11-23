package chapter4

object Exercise4_3 {
  
  /**
   * 
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }
  
  def main(args: Array[String]): Unit = {
    assert(map2(Some(1), None)(_ + _) == None)
    assert(map2(None: Option[Int], Some(1))(_ + _) == None)
    assert(map2(None: Option[Int], None)(_ + _) == None)
    assert(map2(Some(1), Some(2))(_ + _) == Some(3))
    println("All tests successful")
  }
}