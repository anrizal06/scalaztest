package riviera
import scalaz._

object FunctorExample {
   def main(args:Array[String]) = {
      
      import Scalaz._

      val xEit:Either[String,Int] = Right(9)

      println(xEit map incr map incr)

      println(xEit map double map incr)

      val f = incr(_)

      val g = double(_)

//      println( (f map f map f map g map g)(5) )

      
   }


   def incr(x:Int) = x + 1

   def double(x:Int) = x * 2
}
