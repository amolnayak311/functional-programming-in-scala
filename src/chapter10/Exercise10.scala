package chapter10

import chapter10.WordCount._


object Exercise10 {
  
    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      
        /**
         * 
         */
        def zero = Stub("")
        
        /**
         * 
         */
        def op(op1: WC, op2: WC) = (op1, op2) match {
          case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
          case (Stub(s1), Part(l, count, r)) => Part(s1 + l, count, r)
          case (Part(l, count, r), Stub(s1)) => Part(l, count, r + s1)
          case (Part(l1, count1, r1), Part(l2, count2, r2)) => {
            val c = if((r1 + l2).isEmpty) 0 else 1
            Part(l1, count1 + count2 + c, r2)
          }
              
        }
        
    }
}