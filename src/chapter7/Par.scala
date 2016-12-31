package chapter7

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import java.util.concurrent.Executors
import java.util.concurrent.TimeoutException


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
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
    
    
    /**
     * 
     */
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = 
      ps match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }
    
    /**
     * 
     */
    def map[A, B](seq: Par[A])(f: A => B): Par[B] = 
      map2(seq, unit(()))((a, _) => f(a))
      
     
    /**
     * 
     */
    def parMap[A, B](seq : List[A])(f: A => B): Par[List[B]] = fork(sequence(seq.map(asyncF(f))))
      
    
    /**
     * 
     */
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val m = sequence(as map (asyncF(a => if(f(a)) List(a) else List.empty[A])))
      map(m)(_.flatten)
    }
    
    
    /**
     * 
     */
    def map2[A,B,C](a: Par[A], b: Par[B], timeout : Long = -1)(f: (A,B) => C): Par[C] = 
    es => {
      //Implementation that doesn't honor the timeouts yet     
      val af = a(es)
      val bf = b(es)
      //val toProvided = timeout > 0
      //val st = System.currentTimeMillis()
      //val aRet = af.get(timeout, TimeUnit.MILLISECONDS)
      //val et = System.currentTimeMillis();
      //val bRet = bf.get(timeout - et + st, TimeUnit.MILLISECONDS)
      //UnitFuture(f(af.get, bf.get))
      UnitFuture(f(af.get, bf.get))
    }
      
      
    
    
    /**
     * 
     */
    def run[A](es: ExecutorService)(p : Par[A]): Future[A] = p(es)
    
      
   // def getDelayedPar[A](msg: String, delay: Long, retVal:A):Par[A] =
   //   fork(e => { println(msg); Thread.sleep(delay); UnitFuture(retVal)})
    
    def main(args: Array[String]): Unit = {
      // Since one thread of fork is busy waiting for the inner implementation of the
      // par to be invoked, fixed size thread pool will go into dead lock if enough
      // threads are not available in the pool
      
      val es = Executors.newCachedThreadPool()
     // println(map2(getDelayedPar("Getting 1", 2000, 1), getDelayedPar("Getting 2", 10, 2), 100)(_ + _)(es))
      val list = Range(1, 10).toList
      val res = parFilter(list)( _ % 2 == 0)(es).get
      println(res)      
      es.shutdown()
    }
    
    
    //TODO: Implement the Actor based implementations, section 7.4 onwards later
  
}