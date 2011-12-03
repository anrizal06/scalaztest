package org.riv.scalaztest

import org.scalatest.FunSuite
import scalaz._

class ApplicativeFunctorTestSuite extends FunSuite {

   import Scalaz._

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

   }

   test("Lifting Option") { 
      val xOpt = some(3)
      val yOpt = some(5)
      
      val xNone = none
      val AOPT = Applicative[Option]

      expect(Some(8)) {
         val add=(x:Int,y:Int) => x + y

         // The type annotation is not necessary. Just to give more information.
	 val liftedAdd:(Option[Int], Option[Int])=>Option[Int] = AOPT.lift2(add)
         liftedAdd(xOpt, yOpt)
      }

      expect(None) {
         val mul=(x:Int,y:Int) => x * y 
         AOPT.lift2(mul)(xOpt, xNone)
      }

   }

   test("Applicative Functor List") {
      val xs = List(3, 4, 5)
      val ys = List(5, 10, 15)

      val ALS = Applicative[List]

      val incr:Int=>Int = 1 +  
      val double:Int=>Int = 2 *

      expect(List(4, 5, 6, 6, 8, 10)) {
         xs <*> List(incr, double)
      }

      expect(Nil) {
         xs <*> Nil
      }

      val add = (x:Int,y:Int) => x + y
      val fs = ys.map(add.curried(_))

      expect(List(8, 9, 10, 13, 14, 15, 18, 19, 20)) {
         xs <*> fs 
      }

      expect(List(8, 13, 18, 9, 14, 19, 10, 15, 20)) {
         ALS.ap2(xs, ys)(ALS.pure(add))
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

   def apF[F[_],A,B](el:F[A], f:A=>B)(implicit AP:Applicative[F]) = {
      val liftedF = AP.apF(AP.pure(f))
      liftedF(el)
   } 
}

