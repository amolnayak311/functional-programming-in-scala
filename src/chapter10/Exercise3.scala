package chapter10

object Exercise3 {
  
    def endomonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      
      /**
       * 
       */
        def zero: A => A = a => a
        
          /**
           * 
           */
        def op(op1: A => A, op2: A => A): A => A = a => op1(op2(a))
    }
    
    
    def main(args: Array[String]): Unit = {
      val f = endomonoid.op((x:Int) => x * x, (y:Int) => y + 2)
      assert(f(2) == 16)
      println("Tests successful")
    }
}