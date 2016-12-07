package chapter5

import chapter5.Stream._
object Unfold {
  
  //Implement Exercise 5.11, 5.12, 5.13 (tests, implementation in StreamImpl)
  
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
  def map[X, Y](s : Stream[X])(f: X => Y): Stream[Y] = unfold(s){
        case Cons(x, y) => Some((f(x()), y()))
        case Empty => None 
  }
  
      
  /**
   * 
   */
  def take[A](s: Stream[A], n: Int): Stream[A] = unfold((s, n)) {
        case (Cons(x, y), nElems) if nElems > 0 => Some((x(), (y(), nElems - 1)))
        case _  => None
  }
    
  /**
   * 
   */  
  def takeWhile1[A](s:Stream[A])(f : A => Boolean): Stream[A] = unfold(s) {
    case Cons(h, t) if f(h()) => Some((h(), t()))
    case _ => None
  }
  
  /**
   * 
   */
  def zipWith[A,B,C](s1: Stream[A], s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((s1, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2())) 
        case _ => None
  }
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    assert(from(1).take(5).toList == List(1, 2, 3, 4, 5))
    assert(constant('a').take(5).toList == List('a', 'a', 'a', 'a', 'a'))
    assert(ones.take(5).toList == List(1, 1, 1, 1, 1))
    assert(fib.take(5).toList == List(0, 1, 1, 2, 3))
    val s = Stream(1, 2, 3, 4)
    assert(map(s)(_ + 1).toList  == List(2, 3, 4, 5))
    
    assert(take(s, 0).toList == Nil)
    assert(take(s, -1).toList == Nil)
    assert(take(s, 10).toList == List(1, 2, 3, 4))
    assert(take(s, 2).toList == List(1, 2))
    
    assert(takeWhile1(s)(_ > 10).toList == Nil)    
    assert(takeWhile1(s)(_ > 2).toList == Nil)
    assert(takeWhile1(s)(_ < 3).toList == List(1, 2))
    
    assert(zipWith(Stream(1, 2, 3), Stream(1, 2))(_ + _).toList == List(2, 4))
    assert(zipWith(Stream(1, 2), Stream(1, 2, 3))(_ + _).toList == List(2, 4))
    assert(zipWith(Empty: Stream[Int], Stream(1, 2, 3))(_ + _).toList == Nil)
    assert(zipWith(Stream(1, 2, 3), Empty)(_ + _).toList == Nil)
    assert(zipWith(Stream(1, 2, 3), Stream(1, 2, 3))(_ + _).toList == List(2, 4, 6))
    
    println("All tests successful")
  }
}