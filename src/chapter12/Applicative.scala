package chapter12

import chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  
    /**
     * Defined in terms of apply
     */
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = 
      apply(map(fa)(f.curried))(fb)
 
    /**
     *  map3 using apply
     */
      //TODO: Can we implement using map2?
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = 
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
      
    /**
     * 
     */
    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
 
      
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


trait Monad[F[_]] extends Applicative[F] {
  
  /**
   * 
   */
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  
  
  /**
   * 
   */
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(a => a)
  
  
  /**
   * 
   */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
  
  
  /**
   * 
   */
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
  

  /**
   * 
   */
  override def map2[A, B, C](fa: F[A], fb:F[B])(f: (A, B) => C): F[C] = 
    flatMap(fa)(a => map(fb)(b => f(a, b)))
  
  
}