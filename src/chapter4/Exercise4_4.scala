package chapter4

object Exercise4_4 {
  
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    // If any of the element is None, flatMap on it will return None
    // If it isnt, then we simply put the element ahead of the sequence on remaining
    //of the list
    case h :: t => h flatMap(a => sequence(t).map(a :: _ ))
  }
    
    
  
  def main(args: Array[String]): Unit = {
    assert(sequence(List(Some(1), Some(2))) == Some(List(1, 2)))
    assert(sequence(List(Some(1), None)) == None)
    assert(sequence(List(None, Some(1))) == None)
    assert(sequence(List(Some(1), None, Some(2))) == None)
    println("Tests successful")
  }
}