package chapter3

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List {
  
  /**
   * 
   */
  def apply[A](a: A*): List[A] = 
      if(a.isEmpty) Nil else Cons(a.head, apply(a.tail : _*))
      
  /**
   *    
   */
  def empty[A]: List[A] = Nil
      
  /**
   * 
   */
  def foldRight[A, B](l: List[A], b: B)(f: (A, B) => B) : B = l match {
      case Nil   => b
      case Cons(h, t) => f(h, foldRight(t, b)(f))
  }  
  
  
  /**
   * 
   */
  def sum(l: List[Int]): Int = ???
  
}