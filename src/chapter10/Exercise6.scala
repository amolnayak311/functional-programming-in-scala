package chapter10

import chapter10.Exercise5.foldMap
import chapter10.Exercise3.endomonoid


object Exercise6 {

  
    /**
     * 
     */
    def flip[A](m: Monoid[A]) = new Monoid[A] {
      
        /**
         * 
         */
        def zero: A = m.zero
        
        def op(op1: A, op2: A): A = m.op(op2, op1)
    }
    /**
     * 
     */
    def foldLeft[A, B](as: List[A])(zero: B)(f: (B, A) => B): B =
     foldMap(as, flip(endomonoid[B]))((a: A) => (b:B) => f(b, a))(zero)
  
    // The explanation for the above function goes as follows
    // endomonoid is a monoid for a function of type B => B
    // Since the monoid passed for foldMap is of type B => B and the list is of type A
    // We would need the function passed to the foldMap be a higher order function of type
    // A => (B => B)
    // We construct this function using our provide function f as (a: A) => (b:B) => f(b, a)
    // the zero value needs to be a flipped monoid because we need to do zero op elem of list
    // The fold map will return a function B => B, similar to the type of the monoid passed
    // which in this case is an endomonoid
    // Finally we invoke this function using the zero element to get the resulting value B
      
        
    
    
    /**
     * 
     */
    def foldRight[A, B](as: List[A])(zero: B)(f: (A, B) => B): B = 
      foldMap(as, endomonoid[B])(f.curried)(zero)
    
    
    val subtractionMonoid = new Monoid[Int] {
      def zero = 0
      def op(op1: Int, op2:Int) = op1 - op2
    }
    
    def main(args: Array[String]): Unit = {
        val nums = List(1, 2, 3, 4)
        assert(nums.foldLeft(0)(_ - _) == foldLeft(nums)(0)(_ - _))
        assert(nums.foldRight(0)(_ - _) == foldRight(nums)(0)(_ - _))
        println("Tests successful")
    }
}