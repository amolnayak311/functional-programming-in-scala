package chapter5

import Stream.cons
// Implementation of Exercise 5.1, 5.2, 5.3, 5,4
sealed trait Stream[+A] {
  
    /**
     * Converts the Stream to a List
     */
    def toList:List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
    
    /**
     * Take n elements from the list
     */
    def take(n: Int): Stream[A] = this match {      
      case Cons(head, _)  if n == 1 => cons(head(), Empty)      
      case Cons(head, tail) if n > 1 => cons(head(), tail().take(n - 1))
      //Taking negative,0, more than the number of elements of the list 
      //or taking elements from an empty stream will return an empty stream
      case _ => Empty
    }
    
    
    /**
     * 
     */
    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f(h()) =>  cons(h(), t().takeWhile(f)) 
      case _ => Empty  
    }
    
    /**
     * Drop while the condition holds true
     */
    def dropWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f(h()) => t().dropWhile(f)
      case _ => this
    }
    
    /**
     * functions holds true for all elements of the Stream, empty streams always return 
     * true
     */
    def forall(f : A => Boolean): Boolean = this dropWhile f match {
      case Empty => true
      case _ => false
    }
    
    
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  
  /**
   * 
   */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // Make it lazy val so that it doesn't get evaluated everytime we call it 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  
  /**
   * 
   */
  def apply[A](args: A*): Stream[A] = {
    if(args.isEmpty) Empty else cons(args.head, apply(args.tail:_*))
  }
  
  
  
}

object StreamImpl {
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
      val str1 = Stream(1, 2, 3, 4)
      
      //toList test
      assert(str1.toList == List(1, 2, 3, 4))
      assert(Empty.toList == Nil)
      
      //take test      
      assert(str1.take(1).toList == List(1))
      assert(str1.take(0).toList == Nil)
      assert(str1.take(-1).toList == Nil)
      assert(str1.take(5).toList == str1.toList)
      assert(str1.take(4).toList == str1.toList)
      assert(str1.take(3).toList == List(1, 2, 3))
      
      //takeWhile test
      assert(str1.takeWhile(_ > 6).toList == Nil)
      assert(str1.takeWhile(_ < 6).toList == str1.toList)
      assert(str1.takeWhile(_ < 3).toList == List(1, 2))
      
      //Dropwhile test
      assert(str1.dropWhile(_ > 6).toList == str1.toList)
      assert(str1.dropWhile(_ < 6).toList == Nil)
      assert(str1.dropWhile(_ < 3).toList == List(3, 4))
      
      //for all test
      assert(!str1.forall(_ < 3))
      assert(str1.forall(_ < 5))
      assert(!str1.forall(_ > 10))
      assert(Empty.forall(x => false))
      assert(Empty.forall(x => true))
      
      
      println("All tests successful")
  }
}
