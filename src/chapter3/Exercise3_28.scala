package chapter3

object Exercise3_28 {
  
  /**
   * 
   */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Empty() => Empty()
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
  
  def main(args: Array[String]): Unit = {
    assert(map(Leaf(1))(_.toString) == Leaf("1"))
    assert(map(Branch(Leaf(1), Leaf(2)))(_.toString) == Branch(Leaf("1"), Leaf("2")))
    println("All tests successful")        
  }
}