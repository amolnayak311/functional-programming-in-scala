package chapter2

object Exercise2_3 {
  
  /**
   * 
   */
  def curry[A, B, C](f : (A, B) => C): A => (B => C) = x => y => f(x, y)
  
  def main(args: Array[String]): Unit = {
    val addNumbers = curry((a: Int, b: Int) => a + b)
    assert(addNumbers(2)(3) == 5)
    println("Test Successful")
  }  
}