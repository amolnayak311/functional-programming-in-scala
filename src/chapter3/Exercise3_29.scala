package chapter3

object Exercise3_29 {
  
  // Taken from https://github.com/fpinscala
  
  
  /* 
  Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively
  accumulates some value using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use
  this function to implement just about any recursive function that would otherwise be defined by pattern matching.
  */
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  
  /**
   * 
   */
  def sizeViaFold[A](t: Tree[A]): Int = 
    fold(t)(a => 1)(1 + _ + _)
  
    /**
     * 
     */
  def maximumViaFold(t: Tree[Int]): Int = 
    fold(t)(a => a)(_ max _)
  
    /**
     * 
     */
  def depthViaFold[A](t: Tree[A]): Int = 
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))
  
  
  def main(args: Array[String]): Unit = {
    assert(maximumViaFold(Leaf(1)) == 1)
    assert(maximumViaFold(Branch(Leaf(1), Leaf(10))) == 10)
    //TODO: Test others and work on an alternate implementation
    println("All tests successful")
  }
}