package chapter5

import chapter5.Stream._
object Unfold {
  
  //Implement Exercise 5.11 and 5.12
  
  /**
   * 
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => Empty
  }
  
  /**
   * Implement another version of from using unfold
   */      
  def from(f: Int): Stream[Int] = unfold(f)(x => Some(x, x + 1))
  
  /**
   * 
   */
  def constant[A](a: A) = unfold(a)(x => Some(a, a))
  
  /**
   * 
   */
  def ones: Stream[Int] = constant(1)
  
  /**
   * 
   */
  def fib = unfold((0, 1)){ case (n0, n1) => Some((n0, (n1, n0 + n1))) }
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    assert(from(1).take(5).toList == List(1, 2, 3, 4, 5))
    assert(constant('a').take(5).toList == List('a', 'a', 'a', 'a', 'a'))
    assert(ones.take(5).toList == List(1, 1, 1, 1, 1))
    assert(fib.take(5).toList == List(0, 1, 1, 2, 3))
    println("All tests successful")
  }
}