package chapter2

object Exercise2_5 {
  
  /**
   * 
   */
   def compose[A, B, C](f: B => C, g : A => B) : A => C = x => f(g(x))
   
   def main(args: Array[String]): Unit = {
     def square(x: Int) = x * x
     val raise4 = compose(square, square)
     assert(raise4(4) == 256)
     println("Tests Successful")
   }
}