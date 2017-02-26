package chapter11

object Exercise17 {
  
  case class Id[A](value: A) {
    
    /**
     * 
     */
    def map[B](f: A => B): Id[B] = Id(f(value))
    
    /**
     * 
     */
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }
  
  def main(args: Array[String]): Unit = {
    
  }
}