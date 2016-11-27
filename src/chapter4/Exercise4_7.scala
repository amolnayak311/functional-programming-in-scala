package chapter4

object Exercise4_7 {
    
  
  /**
   * 
   */
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: tail => f(h) flatMap {x => traverse(tail)(f) map {x :: _}}
  }
  
  
  /**
   * 
   */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)
  
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    
      assert(sequence(List(Left("E1"), Right(2), Right(3))) == Left("E1"))
      assert(sequence(List(Left("E1"), Left("E2"), Right(3))) == Left("E1"))
      assert(sequence(List(Right(1), Right(2), Right(3))) == Right(List(1, 2, 3)))
      println("All tests successful")
  }
  
}