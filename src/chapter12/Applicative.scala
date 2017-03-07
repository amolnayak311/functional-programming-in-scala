package chapter12

import chapter11.Functor

/**
 * 
 */
sealed trait Validation[+E, +A]

/**
 * 
 */
case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

/**
 * 
 */
case class Success[A](a: A) extends Validation[Nothing, A]

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
    def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = 
      ofa.foldLeft(unit(Map.empty[K, V])) {
        case (acc, (k, v)) => map2(v, acc)((v, map) => map + (k -> v))
      }

    
    
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
    
    
    /**
     * 
     */
    def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = 
      new Applicative[({type f[x] = Validation[E, x]})#f]{
      
          /**
           * 
           */
          def unit[A](a: => A): Validation[E, A] = Success(a)
          
          /**
           * 
           */
          override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C) = 
            (fa, fb) match {
              case (Success(a), Success(b)) => Success(f(a, b))
              case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h1) ++ t2)
              case (x@Failure(_, _), _) => x
              case (_, x@Failure(_, _)) => x
            }
      }
    
    
      def streamApplicative: Applicative[Stream] = new Applicative[Stream]{
        
          /**
           * 
           */
          def unit[A](a: => A): Stream[A] = Stream.continually(a)
          
          /**
           * 
           */
          override def map2[A, B, C](sa: Stream[A], sb: Stream[B])(f: (A, B) => C) = 
            (sa zip sb) map f.tupled
        
      }
      
      /**
       * 
       */
      def product[G[_]](g: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
        val s = this
        new Applicative[({type f[x] = (F[x], G[x])})#f] {
            /**
         		 * 	
          	 */
            def unit[A](a: => A): (F[A], G[A]) = (s.unit(a), g.unit(a))
            
            /**
             * 
             */
            override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) = 
              (s.apply(fs._1)(p._1), g.apply(fs._2)(p._2))
        }
        
      }
      
      /**
       * 
       */
      def compose[G[_]](g: Applicative[G]):Applicative[({type f[x] = F[G[x]]})#f] = {
        val self = this
        new Applicative[({type f[x] = F[G[x]]})#f] {
          /**
           * 
           */
          def unit[A](a: => A) = self.unit(g.unit(a))
          
          
          /**
           * 
           */
          override def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C): F[G[C]] = 
            self.map2(fga, fgb)(g.map2(_, _)(f))
        }
      }
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

/**
 * 
 */
trait Traverse[F[_]]  { 
  
    /**
   	* 
   	*/
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    
    
    /**
     * 
     */
    def sequence[G[_]:Applicative, A](fa: F[G[A]]): G[F[A]] = traverse(fa)(a => a)
}

object Traverse {
  
    def listTraverse: Traverse[List] = new Traverse[List] {
    
      /**
   		 * 
   		 */
      override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]]  = 
          fa.foldLeft(G.unit(List.empty[B]))((acc, v) => G.map2(f(v), acc)(_ :: _))
    }
    
    
    def optionTraverse: Traverse[Option] = new Traverse[Option] {
      
      /**
       * 
       */
      override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = 
        oa match {
          case Some(a) => G.map(f(a))(Some(_))
          case None  => G.unit(None)
        }
        
    }
    
}

object Monad {
  
    def eitherMonad[E] : Monad[({type f[x] = Either[E, x]}) #f] = 
        new Monad[({type f[x] = Either[E, x]}) # f] {
      
          /**
           * 
           */
          def unit[A](a : => A): Either[E, A] = Right(a)
          
          /**
           * 
           */
          override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
            case Left(b) => Left(b)
            case Right(x) => f(x)
          }
          
    }
  
  
}


