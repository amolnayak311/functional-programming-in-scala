package chapter11

trait Monad[F[_]] extends Functor[F] {
  
    /**
     * 
     */
    def unit[A](a: => A):F[A]
    
    /**
     * 
     */
    def sequence[A](lma: List[F[A]]): F[List[A]] = 
      lma.foldRight(unit(List.empty[A]))((a, b) => map2(a, b)((x, y) => x :: y))
    
      
    /**
     * 
     */
    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la map f)
    
     /**
     * 
     */
    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
    
    
    /**
     * 
     */
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
    
    
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
    
    /**
     * 
     */
    def map2[A, B, C](ma : F[A], mb: F[B])(f: (A, B) => C): F[C] = 
      flatMap(ma)(a => map(mb)(b => f(a, b)))
      
     
    //TODO: How does A => F[Boolean] behave when F is a List
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ???
    
    
    /**
     * Klesli Composition
     */
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = x => flatMap(f(x))(g)
    
    
    /**
     * The solution online helped. The trick is to have _: Unit as the parameter and pass it () 
     * which is for unit value. having _:A doesn't work
     */
    def _flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((x: Unit) => ma, f)(())
    
    
    /**
     * 
     */
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(x => x)
    
    
    /**
     * 
     */
    def _compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = 
      (x:A) => join(map(join(unit(f(x))))(b => g(b)))
     
}