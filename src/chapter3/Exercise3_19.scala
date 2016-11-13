package chapter3

object Exercise3_19 {
  
  /**
   * 
   */
    def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, tail) => if(f(h)) Cons(h, filter(tail)(f)) else filter(tail)(f) 
    }
    
    
    def main(args: Array[String]): Unit = {
      val l = List(1, 2, 3, 4)
      assert(filter(l)( _ > 5) == Nil)
      assert(filter(l)( _ < 5) == l)
      assert(filter(l)( _ < 3) == List(1, 2))
      println("All tests successful")
    }
}