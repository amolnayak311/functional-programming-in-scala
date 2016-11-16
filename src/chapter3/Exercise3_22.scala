package chapter3

object Exercise3_22 {
  
  def addLists(l1: List[Int], l2:List[Int]): List[Int] = (l1, l2) match {
    //Skipping the checks for unequal sized lists, result will be same as smaller sized list
    case (_, Nil) | (Nil, _) => Nil
    case (Cons(h1, tail1), Cons(h2, tail2)) => Cons((h1 + h2), addLists(tail1, tail2))
  }
  
  
  def main(args: Array[String]): Unit = {
    assert(addLists(List(1, 2, 3, 4), List(5, 6, 7, 8)) == List(6, 8, 10, 12))
    assert(addLists(List(1, 2), List(5, 6, 7, 8)) == List(6, 8))
    assert(addLists(List(1, 2, 3, 4), List(5, 6)) == List(6, 8))
    assert(addLists(Nil, Nil) == Nil)
    println("All tests successful")
  }
    
}