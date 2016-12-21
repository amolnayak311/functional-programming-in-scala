package chapter7

trait Par[A] {
  
    /**
     * 
     */
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
    
}

object Par {
  
    /**
     * 
     */
    def unit[A](a: A): Par[A] = ???
    
    
    /**
     * 
     */
    def fork[A](a : => Par[A]): Par[A] = ???
    
    
    /**
     * 
     */
    def lazyUnit[A](a : => A): Par[A] = fork(unit(a))
    
    
    
    
    
    /**
     * 
     */
    def get[A](p : Par[A]): A = ???
        
  
}