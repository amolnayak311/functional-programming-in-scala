package chapter10

object Exercise2 {
  
  /**
   * 
   */  
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      
    /**
     * 
     */
    def op(op1: Option[A], op2: Option[A]): Option[A] = op1 orElse op2
    
    /**
     * 
     */
    def zero:Option[A] = None 
    
    
  }
  
  
  def main(args: Array[String]): Unit = {
      assert(optionMonoid.op(Some(1), optionMonoid.zero) == Some(1))
      assert(optionMonoid.op(None, optionMonoid.zero) == None)
      assert(optionMonoid.op(optionMonoid.zero, Some(1)) == Some(1))
      println("All tests successful")
  }
}