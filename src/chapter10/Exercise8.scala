package chapter10

import chapter10.Exercise7.foldMapV
object Exercise8 {
  
    def ordered(nums: IndexedSeq[Int]): Boolean = {
      /**
       * Tracks Min, max and whether the segment is sorted
       */
       val orderedMonoid = new Monoid[Option[(Int, Int, Boolean)]] {
         
         /**
          * 
          */
         def zero = None
         
         /**
          * 
          */
         def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) = (o1, o2) match {
           case (None, x) => x
           case (x, None) => x
           case (Some((min1, max1, isSorted1)), Some((min2, max2, isSorted2))) => 
             Some(min1 min min2, max1 max max2, isSorted1 && isSorted2 && max1 < min2)
         }
       }
       
       foldMapV(nums, orderedMonoid)(x => Some((x, x, true))).map(_._3).getOrElse(true)
    }
    
    
    def main(args: Array[String]): Unit = {
      assert(ordered(List.empty[Int].toIndexedSeq))
      assert(ordered(List(1, 2, 3, 4 ,5).toIndexedSeq))
      assert(!ordered(List(1, 5, 3, 4 ,5).toIndexedSeq))
      println("Tests Successful")
    }
}