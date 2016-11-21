package chapter4

sealed trait Option[+A] {
  
  /**
   * 
   */
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }
   
    /**
     * 
     */
    def flatMap[B](f : A => Option[B]): Option[B] = this match {
      case None => None
      case Some(s) => f(s)
    }
    
    /**
     * 
     */
    def getOrElse[B >: A](default: => B):B = this match {
      case None => default
      case Some(v) => v
    }
    
    /**
     * 
     */
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case Some(_) => this
    }
    
    /**
     * 
     */
    def filter(f: A => Boolean): Option[A] = this match {
      case None => None
      case Some(v) => if(f(v)) this else None
    }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Exercise4_1 {
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
    assert(Some(1).map(_.toString) == Some("1"))
    assert(None.map(_.toString) == None)
    
    
    assert(Some(1).flatMap(x => Some(x.toString)) == Some("1"))
    assert(None.flatMap(x => Some(x.toString)) == None)
    
    assert(Some(1).getOrElse(2) == 1)
    assert(None.getOrElse(2) == 2)
    
    assert(Some(1).orElse(Some(2)) == Some(1))
    assert(None.orElse(Some(2)) == Some(2))
    
    assert(Some(1).filter(_ < 2) == Some(1))    
    assert(Some(1).filter(_ > 2) == None)
    val o: Option[Int] = None
    assert(o.filter(_ > 2) == None)
    
    println("All tests successful")
  }  
}
