package chapter10

import chapter10.Moniod._
import chapter10.Exercise12.ListFoldable

object Exercise16 {
  
  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]) = new Monoid[(A, B)] {
    
      /**
       * 
       */
      def zero: (A, B) = (a.zero, b.zero)
      
      /**
       * 
       */
      def op(o1: (A, B), o2: (A, B)): (A, B) = (a.op(o1._1, o2._1), b.op(o1._2, o2._2))
  }
  
  def main(args: Array[String]): Unit = {
    
    val addMonoid = new Monoid[Int] {
      
      /**
       * 
       */
      def zero: Int = 0
      
      /**
       * 
       */
      def op(op1: Int, op2: Int) = op1 + op2
    }
    
    
    val prodMonoid = new Monoid[Int] {
      
      /**
       * 
       */
      def zero =1
      
      def op(op1: Int, op2: Int) = op1 * op2
    }
    
    val m = productMonoid(addMonoid, prodMonoid)
    
    val l = List(1, 2, 3, 4)
    
    val (add, prod) = ListFoldable.foldMap(l zip l)(a => a)(m)
    
    assert(add == 10)
    assert(prod == 24)
    println("All tests successful")
    
    
  }
}