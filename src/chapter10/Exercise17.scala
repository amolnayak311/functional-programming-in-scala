package chapter10

object Exercise17 {
  
    def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
      def zero: A => B = a => B.zero
      
      def op(f: A => B, g: A => B) = a => B.op(f(a), g(a)) 
    }
    
    def main(args: Array[String]): Unit = {
      val f: Int => Int = x => x * x
      
      val addMonoid = new Monoid[Int] {
        def zero = 0
        
        def op(o1: Int, o2: Int) = o1 + o2
      }
      
      val fMon: Monoid[Int => Int] = functionMonoid(addMonoid)
      
      assert(fMon.zero(10) == 0)
      assert(fMon.op(f, f)(2) == 8)
      println("All tests successful")
      
    }
    
}