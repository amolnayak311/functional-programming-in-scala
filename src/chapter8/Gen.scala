package chapter8

//Lets implement RNG and State we had in chapter 6 again as a refresher

trait RNG[A] {
  
    /**
   	* 
   	*/
    def next: (A, RNG[A])
    
}

/**
 * 
 */
object RNG {
    /**
     * 
     */
    def boolean(r : RNG[Boolean]): (Boolean, RNG[Boolean]) = ???
    
    /**
     * 
     */
    def int(r: RNG[Int]): (Int, RNG[Int]) = ???
}


case object IntGenerator extends RNG[Int] {
  /**
   * 
   */
  def next: (Int, RNG[Int]) = ???
}

case object BooleanGenerator extends RNG[Boolean] {
  
  /**
   * 
   */
  def next: (Boolean, RNG[Boolean]) = ???
}

/**
 * 
 */
class State[S, A](run: S => (A, S))

/**
 * 
 */
object State {

    /**
     * 
     */
    def unit[A, S](a: => A): State[S, A] = ???
}



class Gen[A](sample: State[RNG[A], A])



object Gen {
  
    /**
     * 
     */
    def unit[A](a: => A): Gen[A] = new Gen(State.unit(a))
    
    
    /**
     * 
     */
    def boolean: Gen[Boolean] = new Gen(new State(RNG.boolean))
    
    
}