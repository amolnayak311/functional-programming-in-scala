package chapter10

object Test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(76); 
  println("Welcome to the Scala worksheet");$skip(66); 
  
  def f: Int => (String => Int ) = x => (str => x + str.toInt);System.out.println("""f: => Int => (String => Int)""");$skip(7); val res$0 = 
  f(1);System.out.println("""res0: String => Int = """ + $show(res$0))}
  
}
