package chapter6

import State._
class State[+A, S](val run : S => (A, S)) {
    
  /**
   * 
   */
   def flatMap[B](f: A => State[B, S]): State[B, S] = 
       new State(s => {       
         val (a, ns) = run(s)
         f(a).run(ns)        
       }
     )
   
   
   /**
    * 
    */
   def map[B](f: A => B): State[B, S] = flatMap(x => unit(f(x)))
   
   
   /**
    * 
    */
   def map2[B, C](b: State[B, S])(f: (A, B) => C): State[C, S] = flatMap(a => b.map(f(a, _))) 
     
  
}

object State {
  
  
  /**
   * 
   */
  def unit[S, T](a: T): State[T, S] = new State(s => (a, s))
  
  /**
   * 
   */
  def main(args: Array[String]): Unit = {
      
  }
}