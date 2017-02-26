package chapter12

import chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  
    /**
     * Defined in terms of apply
     */
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = 
      apply(map(fa)(f.curried))(fb)
    
      
    /**
     * Defined in terms of apply and unit
     */
    def map[A, B](fa: F[A])(f: A => B): F[B] = 
        apply(unit(f))(fa)
      
      
     /**
     *  Defined in terms of map2 
     */
    def _map[A, B](fa: F[A])(f: A => B): F[B] = map2(unit(()), fa)((_, b) => f(b))
    
    /**
     * 
     */
    def unit[A](a: => A): F[A]
    
    
    /**
     * Implement apply in terms of map2
     */
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))
    
    
    
    
    
    /**
     * 
     */
    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = 
      as.foldLeft(unit(List.empty[B]))((a, b) => map2(f(b), a)(_ :: _))
      
      
    /**
     * 
     */
    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(a => a)
    
    /**
     * 
     */
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
    
    /**
     * 
     */
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =  map2(fa, fb)((_, _))
    
    
    
    
}