package chapter10
import chapter10.Exercise6._
import chapter10.Exercise3.endomonoid


/**
 * Implementation of exercise 3.12, 3.14  and 3.15 
 */
object Exercise12 {
  
  trait Foldable[F[_]] {
    
    /**
     * 
     */
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = 
      foldMap(as)(f.curried)(endomonoid[B])(z)
    
    
      
   /**
    * 
    */
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B) : B = 
      foldMap(as)((a:A) => (b:B) => f(b, a))(flip(endomonoid[B]))(z)
      
    
    /**
     * 
     */
    def foldMap[A, B](as: F[A])(f : A => B)(mb : Monoid[B]): B 
    
    
    /**
     * 
     */
    def toList[A](fa: F[A]): List[A] = foldRight(fa)(Nil: List[A])(_ :: _)
      
    
  }
  
  
  object ListFoldable extends Foldable[List] {
    
    /**
     * 
     */
    def foldMap[A, B](as: List[A])(f : A => B)(mb : Monoid[B]): B = 
      as.foldLeft(mb.zero)((a, e) => mb.op(a, f(e)))
    
  }
  
  //TODO: Implementing this for IndexedSequence and Stream in a similar way
  //making use of their respective foldLeft implementation
  
  
  object OptionFoldable extends Foldable[Option] {
    
    def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case None => mb.zero
      case Some(x) => f(x)
    }
  }
  
  def main(args: Array[String]): Unit = {
    
    val strMonoid = new Monoid[String] {
        def zero:String = ""
        
        def op(o1: String, o2: String): String = o1 + o2
    }
    
    val l = List(1, 2, 3, 4)
    assert(ListFoldable.foldRight(l)(0)(_ - _) == -2)
    assert(ListFoldable.foldLeft(l)(0)(_ - _) == -10)
    assert(ListFoldable.foldMap[Int, String](l)(_.toString)(strMonoid) == "1234")
    assert(ListFoldable.toList(l) == l)
    
    
    // 
    val none = None
    val some = Some(1)
    assert(OptionFoldable.foldMap(some)(_.toString)(strMonoid) == "1")
    assert(OptionFoldable.foldMap(none)(_.toString)(strMonoid).isEmpty)
    assert(OptionFoldable.foldLeft(none: Option[String])(":")((a, b) => a + b) == ":")
    assert(OptionFoldable.foldRight(none: Option[String])(":")((a, b) => a + b) == ":")
    assert(OptionFoldable.foldLeft(some)(":")((a, b) => a + b) == ":1")
    assert(OptionFoldable.foldRight(some)(":")((a, b) => a + b) == "1:")    
    assert(OptionFoldable.toList(some) == List(1))
    
    
    println("All tests successful")
  }
}