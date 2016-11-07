package chapter3
import chapter3.List._

object Exercise3_1 {
  
  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101      
    }
    
    println("Value of x is " + x)
  }
}