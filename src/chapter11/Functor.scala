package chapter11

trait Functor[F[_]] {
  
    /**
     * 
     */
    def map[A, B](fa: F[A])(a: A => B): F[B]
    
    /**
     * 
     */
    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = 
      (map(fab)(_._1), map(fab)(_._2))
      
    
    /**
     * 
     */
    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
      case Left(l) => map(l)(Left(_))
      case Right(r) => map(r)(Right(_))
    }
}