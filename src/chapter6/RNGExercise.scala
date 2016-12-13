package chapter6

trait RNG {
  /**
   * 
   */
  def nextInt: (Int, RNG)
}

/**
 * Exercise 6.1
 */

case class SimpleRNG(seed: Long) extends RNG {  
  
    /**
     * 
     */
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      ((newSeed >>> 16).toInt, SimpleRNG(newSeed))      
    }
    
    /**
     * 
     */
    def nonNegativeInt: (Int, RNG) = {
      val (next, nextState) = nextInt
      (if (next < 0) -(next + 1) else next, nextState)
    }
    
    /**
     * 
     */
    def double: (Double, RNG) = {
      val (next, nextState) = nonNegativeInt
      (next.toDouble / Int.MaxValue, nextState)
    }
  
}


object RNGExercise {
  
  def main(args: Array[String]): Unit = {
    val r = SimpleRNG(Long.MaxValue)
    val (next, _) = r.nextInt    
    assert(r.nonNegativeInt._1 == -(next + 1))
    assert(r.double._1 <= 1)
    assert(r.double._1 == - (next + 1).toDouble / Int.MaxValue)
    println("All tests successful")
  }
}