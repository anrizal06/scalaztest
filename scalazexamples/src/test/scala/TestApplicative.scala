package org.riv.scalaztest

import org.scalatest.FunSuite
import scalaz._

class ApplicativeFunctorTestSuite extends FunSuite {

   import Scalaz._
   
   val incr:Int=>Int = 1 +  
   
   val double:Int=>Int = 2 *
   
   val add = (x:Int,y:Int) => x + y
   
   val mul = (x:Int,y:Int) => x * y
   
   val add3 = (x:Int,y:Int,z:Int) => x + y + z

   test("Test ap Option") {
      val xOpt = some(3)
      val yOpt = some(5)
      val xNone = none

      val incr:Int=>Int = 1 +

      val AOPT = Applicative[Option]

      expect(Some(4)) {      
         xOpt <*> some(incr)
      }

      expect(None) {
         xNone <*> some(incr)
      }

      expect(None) {
         yOpt <*> none
      }
      
      expect(Some(8)) {
    	 AOPT.ap2(xOpt, yOpt)(AOPT.pure(add))
      }
      
      // This is equivalent to AOPT.ap2(xOpt, yOpt)(AOPT.pure(add))
      expect(Some(8)) {
    	  (xOpt <**> yOpt)(add)
      }
      
      expect(None) {
    	 AOPT.ap2(xOpt, xNone)(AOPT.pure(mul))
      }
      
      expect(Some(11)) {
        AOPT.ap3(xOpt, yOpt, xOpt)(AOPT.pure(add3))
      }
      
      expect(Some(11)) {
        (xOpt <***> (xOpt, yOpt))(add3)
      }
      
      expect(yOpt) {
        xOpt *> yOpt
      }
      
      expect(xOpt) {
        xOpt <* yOpt
      }
      
      expect(None) {
        xOpt <* xNone
      }
      
      expect(None) {
        xOpt *> xNone
      }
      
      expect(Some(3,5)) {
    	xOpt <|*|> yOpt
      }
      
      expect(None) {
    	xOpt <|*|> xNone
      }
   }
   
   test("Applicative Functor List") {
      val xs = List(3, 4, 5)
      val ys = List(5, 10, 15)
      val zs = List(1, 2)

      val AP = Applicative[List]

      expect(List(4, 5, 6, 6, 8, 10)) {
         xs <*> List(incr, double)
      }

      expect(Nil) {
         xs <*> Nil
      }

      // fs contains a partially applied function add by each element of ys
      val fs = ys.map(add.curried(_))

      expect(List(8, 9, 10, 13, 14, 15, 18, 19, 20)) {
         xs <*> fs 
      }

     expect(List(8, 13, 18, 9, 14, 19, 10, 15, 20)) {
         AP.ap2(xs, ys)(AP.pure(add))
      }
      
      expect(List(8, 13, 18, 9, 14, 19, 10, 15, 20, 15, 30, 45, 20, 40, 60, 25, 50, 75)) {
         AP.ap2(xs, ys)(List(add, mul))
      }
      
      expect(List(9, 10, 14, 15, 19, 20, 10, 11, 15, 16, 20, 21, 11, 12,  16, 17, 21, 22)) {
        AP.ap3(xs,ys,zs)(List(add3))
      }
      
      expect(List(1,2,1,2,1,2)) {
        xs *> zs
      }
      
      expect(List(3, 3, 4, 4, 5, 5)) {
        xs <* zs
      }
      
      expect(List((3,5), (3,10), (3,15), (4, 5), (4, 10), (4, 15), (5, 5), (5, 10) , (5, 15))) {
        xs <|*|> ys
      }
   }



   test("Lifting Option") { 
      val xOpt = some(3)
      val yOpt = some(5)
      
      val xNone = none
      val AOPT = Applicative[Option]

      expect(Some(8)) {
         // The type annotation is not necessary. Just to give more information.
    	 val liftedAdd:(Option[Int], Option[Int])=>Option[Int] = AOPT.lift2(add)
         liftedAdd(xOpt, yOpt)
      }

      expect(None) {
         AOPT.lift2(mul)(xOpt, xNone)
      }

   }

   test("Applicative appF") {
      val incr:Int=>Int = 1 + 
      
      val xOpt = some(1)
      expect(Some(2)) {
        apF(xOpt, incr)
      }

      val xs = List(2, 4, 5)
      expect(List(3, 5, 6)) {
        apF(xs, incr)
      }

      val ys = Stream.from(10)
      expect(List(11,12,13)) {
        apF(ys, incr).take(3).toList
      }
	
   }

   test("Applicative Either") {
      val incr:Int=>Int = 1 +
      
      val xEith:Either[String,Int] = Right(1)

      type EitherString[A] = ({type L[a]=Either[String, a]})#L[A]
      val AP = Applicative[EitherString]
 
      expect(Right(2)) {
         AP.ap(xEith)(AP.pure(incr))    
      }

      expect(Left("Error")) {
         AP.ap(Left("Error"))(AP.pure(incr))
      }
   }

   def apF[F[_],A,B](el:F[A], f:A=>B)(implicit AP:Applicative[F]) = {
      val liftedF = AP.apF(AP.pure(f))
      liftedF(el)
   } 
}

