package chapter2

object Exercise2_2 {
  
  /**
   * 
   */
  def isSorted[A](as : Array[A], ordered : (A, A) => Boolean) : Boolean = {
    def isSortedInner(index: Int): Boolean = {
      if(index >= as.length - 1) true
      else if(ordered(as(index), as(index + 1)))
        isSortedInner(index + 1)
        else
          false         
    }    
    isSortedInner(0)
  }
  
  
  def main(args: Array[String]): Unit = {
    val in1 = Array(1, 2, 3, 4)
    assert(isSorted(in1, (f:Int, f1: Int) => f <= f1))
    assert(isSorted(Array.emptyIntArray, (f:Int, f1: Int) => f <= f1))
    assert(isSorted(Array(1), (f:Int, f1: Int) => f <= f1))
    assert(!isSorted(Array(2, 3, 1), (f:Int, f1: Int) => f <= f1))
    assert(isSorted(Array(4, 3, 2, 0), (f:Int, f1: Int) => f >= f1))
    println("All tests passed")
  }
}