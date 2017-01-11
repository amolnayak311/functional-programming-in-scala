package chapter10

import chapter10.Exercise1.intAddition

object Exercise5 {
  
  /**
   * 
   */
    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = 
      as.foldLeft(m.zero)((a, b) => m.op(a, f(b)))
    
    
    def main(args: Array[String]): Unit = {
        val strNumbers = List("1", "2", "3", "4")
        assert(foldMap(strNumbers, intAddition)(_.toInt) == 10)
        println("Tests successful")
    }
}