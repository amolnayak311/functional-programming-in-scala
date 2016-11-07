package chapter3

object Exercise3_3 {
  
  def setHead[A](newHead: A, list: List[A]) : List[A] = list match {
    case Nil => throw new UnsupportedOperationException("setHead on empty list")
    case Cons(_, y) => Cons(newHead, y)
  }
  
  def main(args: Array[String]): Unit = {
    assert(setHead(5, List(1, 2, 3, 4)) == List(5, 2, 3, 4))
    try {
      setHead(5, Nil)
    } catch {
      case e: UnsupportedOperationException => 
        assert(true)
        println("All cases successful")
        return
      case _: Throwable =>   
    }
    assert(false)
  }
}