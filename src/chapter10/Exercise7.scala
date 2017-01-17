package chapter10

import chapter10.Exercise1.intMultiplication

object Exercise7 {
  
  /**
   * 
   */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(v.isEmpty) m.zero
    else {
      if(v.tail.isEmpty) {
         f(v(0))
      }
      else {
        val (l, r) = v.splitAt(v.length / 2) 
        m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    val is = List("1", "2", "3", "4").toIndexedSeq
    assert(foldMapV(is, intMultiplication)(_.toInt) == 24)
    println("All tests successful")
  }
}