package chapter4

import Exercise4_1._

object Exercise4_2 {
  
  /**
   * 
   */
    def mean(s: Seq[Double]): Option[Double] = 
      if(s.isEmpty) None else Some(s.foldLeft(0.0)(_ + _) /s.length) 
    
      /**
       * 
       */
    def variance(seq: Seq[Double]): Option[Double] =      
     mean(seq) flatMap ( m => mean(seq.map (x => math.pow(x -  m, 2) )))
     
     
    def main(args: Array[String]): Unit = {
      assert(variance(List(1, 2, 3, 4, 5)).getOrElse(Double.NaN) == 2)
      assert(variance(List()).getOrElse(Double.NaN).isNaN)
      println("All tests successful")
    }
    
}