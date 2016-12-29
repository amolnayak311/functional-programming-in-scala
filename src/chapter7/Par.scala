package chapter7

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable


object Par {
  
    type Par[A] = ExecutorService => Future[A]
    
    case class UnitFuture[A](get: A) extends Future[A] {
      /**
       * 
       */
       def isDone = true
       
       /**
        * 
        */
       def get(timeout: Long, units: TimeUnit) = get
       
       /**
        * 
        */
       def isCancelled = false
       
       /**
        * 
        */
       def cancel(evenIfRunning: Boolean): Boolean = false
    }
  
    /**
     * 
     */
    def unit[A](a: A): Par[A] = _ => new UnitFuture(a)
    
    
    /**
     * Spawns two threads, first one is when we call es.submit
     * second is invoked for the a instance passed
     */
    def fork[A](a : => Par[A]): Par[A] = es => {
      es.submit(new Callable[A] {
        /**
         * 
         */
        def call: A = a(es).get
      })
    }
    
    
    /**
     * 
     */
    def lazyUnit[A](a : => A): Par[A] = fork(unit(a))
    
    
    /**
     * 
     */
    def map2[A,B,C](a: Par[A], b: Par[B], timeout : Long = -1, unit: TimeUnit = TimeUnit.NANOSECONDS)(f: (A,B) => C): Par[C] = 
    es => {
      //Implementation that doesn't honor the timeouts
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
      
      
    
    
    /**
     * 
     */
    def run[A](es: ExecutorService)(p : Par[A]): Future[A] = p(es)
    
        
  
}