package chapter2

object Exercise2_4 {
  
  /**
   * 
   */
  def uncurry[A, B, C](f : A => B => C): (A, B) => C = (x, y) => f(x)(y)
  
  def main(args: Array[String]): Unit = {
    def curriedAdd(a:Int)(b: Int) = a + b
    val unCurriedAdd = uncurry(curriedAdd)
    assert(unCurriedAdd(2, 3) == 5)
    println("Tests Successful")
  }
}