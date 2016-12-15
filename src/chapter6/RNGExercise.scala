package chapter6

import scala.collection.mutable.ListBuffer

trait RNG {
  /**
   * 
   */
  def nextInt: (Int, RNG)  
  
}

/**
 * Exercise 6.1, 6.2, 6.3, 6.4, 6.5, 6.6
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
    def nonNegativeInt(cState:RNG): (Int, RNG) = {
      val (next, nextState) = cState.nextInt
      (if (next < 0) -(next + 1) else next, nextState)
    }
    
    /**
     * 
     */
    def double(cState: RNG): (Double, RNG) = {
      val (next, nextState) = nonNegativeInt(cState)
      (next.toDouble / Int.MaxValue, nextState)
    }
    
    /**
     * 
     */
    def intDouble(cState: RNG): ((Int, Double), RNG) = {
      val (nInt, nState) = cState.nextInt
      val (nDouble,  nnState) = double(nState)
      ((nInt, nDouble), nnState)
    }
    
    /**
     * 
     */
    def doubleInt(cState: RNG): ((Double, Int), RNG) = {
      val ((in, dou), nState) = intDouble(cState)
      ((dou, in), nState)
    }
    
    
    /**
     * 
     */
    def double3(cState: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng1) = double(cState)
      val (d2, rng2) = double(rng1)
      val (d3, rng3) = double(rng2)
      ((d1, d2, d3), rng3)
    }
    
    /**
     * 
     */
    def ints(count:Int)(rng: RNG):(List[Int], RNG) = { 
      val (res, nextState) = Stream.from(1).takeWhile(_ <= count).foldLeft((ListBuffer.empty[Int], rng)) {
        case ((accValues, cRng), _) => {
          val (intVal, nRNG) = cRng.nextInt
          (accValues += intVal, nRNG)
        }
      }
      (res.toList, nextState)
    }
    
    type Rand[+A] = RNG => (A, RNG)
    
    /**
     * 
     */
    def unit[A](a:A): Rand[A] = rng => (a, rng)
    
    
    /**
     * 
     */
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = 
      rng => {
        val (a, r) = s(rng)
        (f(a), r)
      }
    
    /**
     * 
     */
    def map2[A, B, C](r1: Rand[A], r2: Rand[B])(f: (A, B) => C): Rand[C] = 
      rng => {
        val (a, rs1) = r1(rng)
        val (b, rs2) = r2(rs1)
        (f(a, b), rs2)
      }
      
     /**
      * 
      */
     val int: Rand[Int] = _.nextInt
    
      /**
       * 
       */
    def both[A, B](r1: Rand[A], r2: Rand[B]): Rand[(A, B)] = map2(r1, r2)((_, _))
    
    
    /**
     * 
     */
    def doubleByMap(rng: RNG): (Double, RNG) =
      map(nonNegativeInt)(_.toDouble / Int.MaxValue)(rng)
    
  
    /**
     * 
     */
    val randIntDouble: Rand[(Int, Double)] = both(int, double)
    
    
    /**
     * Could have been both(double, int), but this implementation
     * would not be consistent with the previous implementation
     */
    val randDoubleInt: Rand[(Double, Int)] = rng => {
       val ((i, d), next) = randIntDouble(rng)
       ((d, i), next)
     }
    
    
}


object RNGExercise {
  
  def main(args: Array[String]): Unit = {
    val r = SimpleRNG(Long.MaxValue)
    val (next, _) = r.nextInt    
    assert(r.nonNegativeInt(r)._1 == -(next + 1))
    assert(r.double(r)._1 <= 1)    
    assert(r.double(r)._1 == - (next + 1).toDouble / Int.MaxValue)
    val (expectedDouble, _) = r.double(r.double(r)._2)    
    assert(r.intDouble(r)._1 == (next, expectedDouble))
    assert(r.doubleInt(r)._1 == (expectedDouble, next))
    
    val (v1, r1) = r.nextInt
    val (v2, r2) = r1.nextInt
    val (v3, r3) = r2.nextInt
    assert(r.ints(3)(r) == (List(v1, v2, v3), r3))
    
    assert(r.double(r) == r.doubleByMap(r))
    
    assert(r.intDouble(r) == r.randIntDouble(r))
    assert(r.doubleInt(r) == r.randDoubleInt(r))
    
    println("All tests successful")
    
  }
}