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
  def sum(list: List[Int]): Int = ???
}