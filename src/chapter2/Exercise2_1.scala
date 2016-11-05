package chapter2

import scala.annotation.tailrec

object Exercise2_1 {
  
    /**
     * Find nth Fibonacci number
     */
  
    def fib(n:Int): Int = {
      @tailrec
      def fibInner(nMinusTwo: Int, nMinusOne: Int, n:Int):Int = 
        if(n == 0) nMinusTwo else fibInner(nMinusOne:Int, nMinusTwo + nMinusOne, n - 1)
      fibInner(0, 1, n)  
    }
    
    
    def main(args: Array[String]): Unit = {
      assert(fib(0) == 0)
      assert(fib(1) == 1)
      assert(fib(2) == 1)
      assert(fib(3) == 2)
      assert(fib(40) == 102334155)
      println("Exercise successful")
      
    }
  
}