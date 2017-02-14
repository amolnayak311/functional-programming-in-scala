package chapter11

object Exercise1 {
  
   def optionMonad: Monad[Option] = new Monad[Option] {
     
     /**
      * 
      */
      def unit[A](a: => A): Option[A] = Some(a)
     
      /**
   	  * 
   	  */
      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
         case None => None
        case Some(a) => f(a)
      }
     
   }
   
   def streamMonad: Monad[Stream] = new Monad[Stream]{
     
     /**
      * 
      */
     def unit[A](a: => A): Stream[A] = Stream(a)
     
     def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = 
         fa flatMap f
     
   }
   
   def listMonad: Monad[List] = new Monad[List] {
     
     /**
      * 
      */
     def unit[A](a: => A): List[A] = List(a)
     
     /**
      * 
      */
     def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
         fa flatMap f
     
     
   }
   
   
   def main(args: Array[String]): Unit = {
     
       val om = optionMonad
       val lm = listMonad
       val sm = streamMonad
       assert(om.unit(1) == Some(1))
       assert(lm.unit(1) == List(1))
       assert(sm.unit(1) == Stream(1))
       
       assert(om.map(Option(1))(_ + 1) == Some(2))
       assert(om.map(None: Option[Int])(_ + 1) == None)
       
       assert(lm.map(List(1))(_ + 1) == List(2))
       assert(lm.map(Nil: List[Int])(_ + 1) == Nil)
       
       
       assert(sm.map(Stream(1))(_ + 1) == Stream(2))
       assert(sm.map(Stream.empty[Int])(_ + 1) == Stream.empty[Int])
      
       assert(om.map2(Some(1), Some(2))(_ + _) == Some(3))
       assert(om.map2(None: Option[Int], Some(2))(_ + _) == None)
       assert(om.map2(Some(1), None)(_ + _) == None)
       
       assert(lm.map2(List(1), List(2))(_ + _) == List(3))
       assert(lm.map2(Nil: List[Int], List(2))(_ + _) == Nil)
       assert(lm.map2(List(1), Nil)(_ + _) == Nil)
       
       val eStream = Stream.empty[Int]
       assert(sm.map2(Stream(1), Stream(2))(_ + _) == Stream(3))
       assert(sm.map2(eStream, Stream(2))(_ + _) == eStream)
       assert(sm.map2(Stream(1), eStream)(_ + _) == eStream)

       
       
       println("All tests successful")
   }
}