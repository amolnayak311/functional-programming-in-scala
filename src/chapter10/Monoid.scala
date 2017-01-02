package chapter10

/**
 * 
 * 
 *  1. Moniods are seen everyday
 *  2. For addition or ints 12 have ((a + b) + c) == (a + (b + c))
 *  3. Similarly for addition 0 is an identity element because a + 0 == a
 *  4. For the multiplication operation 1 becomes the identity element
 *  5. For String concatenation, empty string is the identity operation
 *  6. For logical && and || operation, true and false become the identity element respectively 
 * 
 * 
 * 
 */
trait Monoid[A] {
    
  /**
   * 
   */
  def op(op1: A, op2: A): A
  
  /**
   * 
   */
  def zero: A
}

object Moniod {
  
    def stringMonoid = new Monoid[String] {
      /**
       * 
       */
      def op(s1: String, s2: String): String = s1 + s2
      
      
      /**
       * 
       */
      def zero:String = ""        
    }
    
    
    
    def listMonoid[A] = new Monoid[List[A]] {
      
        /**
         * 
         */
         def op(op1: List[A], op2: List[A]): List[A] = op1 ++ op2
         
        /**
         * 
         */
         def zero: List[A] = Nil
    }
    
    
    
}