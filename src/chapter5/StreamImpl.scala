package chapter5

import Stream.{cons, empty}
// Implementation of Exercise 5.1, 5.2, 5.3, 5,4, 5.5
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
    
    /**
     * 
     */
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
    
    /**
     * 
     */
    def takeWhile1(f: A => Boolean): Stream[A] = 
      foldRight(empty[A]){ case (a, b) => if (f(a)) cons(a, b) else empty}
    
    /**
     * 
     */
    def headOption: Option[A] = 
      foldRight(None: Option[A]){case (a, _) => Some(a)}
    
    
    
    /**
     * 
     */
    def map[B](f: A => B): Stream[B] = 
      foldRight(empty[B]){case (a, b) => cons(f(a), b)}
    
    
    /**
     * 
     */
    def filter(f: A => Boolean): Stream[A] = 
      foldRight(empty[A]) {case (a, b) => if(f(a)) cons(a, b) else b }
    
    
    /**
     * 
     */
    def append[B >: A](str: => Stream[B]): Stream[B] = 
      foldRight(str){case (x, y) => cons(x, y)}
    
    
    /**
     * 
     */
    def flatMap[B](f: A => Stream[B]): Stream[B] = ???

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
  
  /**
   * 
   */
  def empty[B]:Stream[B] = Empty
  
  
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
      
      
       //takeWhile1 test
      assert(str1.takeWhile1(_ > 6).toList == Nil)
      assert(str1.takeWhile1(_ < 6).toList == str1.toList)
      assert(str1.takeWhile1(_ < 3).toList == List(1, 2))
      
      
      //headOption using foldRight
      assert(str1.headOption == Some(1))
      assert(Empty.headOption == None)
      
      
      //map operation
      assert(str1.map(x => x * x).toList == List(1, 4, 9, 16))
      assert(str1.map(_.toString).toList == List("1", "2", "3", "4"))
      assert(Empty.map(_.toString).toList == Nil)
      
      //Filter Operation
      assert(str1.filter(_ % 2 == 0).toList == List(2, 4))
      assert(str1.filter(_ > 3).toList == List(4))
      assert(str1.filter(_ > 4).toList == Nil)
      assert(empty[Int].filter(_ > 4).toList == Nil)
      
      //append
      assert(str1.append(Stream(5, 6, 7)).toList == List(1, 2, 3, 4, 5, 6, 7))
      assert(str1.append(empty).toList == List(1, 2, 3, 4))
      assert(empty.append(str1).toList == List(1, 2, 3, 4))
      
      
      println("All tests successful")
  }
}
