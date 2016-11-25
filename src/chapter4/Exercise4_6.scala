package chapter4

sealed trait Either[+E, +A] {
  
  /**
   * 
   */
  def map[B](f: A => B): Either[E, B] = this match {
    case l:Left[_] => l
    case Right(v) => Right(f(v))    
  }
  
  /**
   * 
   */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case l:Left[_] => l
    case Right(v) => f(v)
  }
  
  /**
   * 
   */
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
  }
  
  
  /**
   * 
   */
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Left(x) => Left(x)
    case Right(r) => b map {r1 => f(r, r1)}

    //Alternatively we can do the following
    // for {x <- this ; y <- b} yield f(x, y)    
  }
    
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]




object Exercise4_6 {
  
  def Try[A](ex: => A): Either[Exception, A] = 
  try 
    Right(ex)
  catch {case e:Exception => Left(e)}
  
  
  
  /**
   * 
   */
   def main(args: Array[String]): Unit = {
       Try(1 / 0) match {
         case Left(e) => 
           assert(e.getClass == Class.forName("java.lang.ArithmeticException"))         
         case _ => assert(false)
       }
       
       Try(4 / 2) match {
         case Right(e) => 
           assert(e == 2)         
         case _ => assert(false)
       }
       
       
       println("All tests successful")     
   }
}