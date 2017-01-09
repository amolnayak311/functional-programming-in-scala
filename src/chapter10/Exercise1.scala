package chapter10



object Exercise1 {
  
  
    /**
   	* 
   	*/
    val intAddition: Monoid[Int] =new Monoid[Int] {
        
        /**
         * 
         */
        def op(op1: Int, op2: Int) = op1 + op2
        
        /**
         * 
         */
        def zero: Int = 0
    }
   
    /**
     * 
     */
    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      
        /**
         * 
         */
         def op(op1: Int, op2: Int):Int = op1 * op2
         
         /**
          * 
          */
         def zero:Int = 1
    }
    
    /**
     * 
     */
    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      
        /**
         * 
         */
        def op(op1: Boolean, op2: Boolean): Boolean = op1 || op2
        
        
        /**
         * 
         */
        def zero: Boolean = false
    }
    
    /**
     * 
     */
    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      
        /**
         * 
         */
         def op(op1: Boolean, op2: Boolean): Boolean = op1 && op2
         
         
        /**
         * 
         */
         def zero: Boolean = true
         
    }
    
    
    /**
     * 
     */
    def main(args: Array[String]): Unit = {
      
        val nums = List(1, 2, 3, 4)
        val booleanVals = nums.map(_ > 2)
        
        assert(nums.foldLeft(intAddition.zero)(intAddition.op) == 10)
        assert(nums.foldLeft(intMultiplication.zero)(intMultiplication.op) == 24)
        assert(booleanVals.foldLeft(intMultiplication.zero)(intMultiplication.op) == 24)
        println("All tests successful")
    }
    
}