package chapter4

import scala.util.{Try, Success, Failure}
object Exercise4_5 {
  
  /**
   * 
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case head :: tail => f(head)  flatMap { x => traverse(tail)(f) map {y =>  x :: y}} 
  }
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    assert(traverse(List("1", "2", "3"))(x => Some(x.toInt)) == Some(List(1, 2, 3)))
    val res = traverse(List("1", "a", "3")){
        x => Try(x.toInt) match {
          case Success(s) => Some(s)
          case Failure(_) => None  
      }      
    }
    assert(res == None)
    println("All tests successful")
  }
    
}