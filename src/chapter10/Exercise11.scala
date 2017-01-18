package chapter10

import chapter10.Exercise10._
import chapter10.WordCount._
import chapter10.Exercise7.foldMapV

object Exercise11 {
  
    /**
     * 
     */
    def count(s: String): Int = foldMapV(s.toIndexedSeq,wcMonoid)(x => 
      if(x.isWhitespace)
        Part("", 0, "")
       else 
        Stub(x.toString)) match {
      case Stub(_) => 0
      case Part(_, count, _) => count
    }
    
    
    def main(args: Array[String]): Unit = {
      assert(count("") == 0)
      assert(count("    ") == 0)
      assert(count("  Test String  ") == 2)
      assert(count("Test    String") == 2)
      assert(count("Test") == 1)
      println("All tests successful")
    }
}