package org.riv.scalaztest

import org.scalatest.FunSuite 
import scalaz._

// A test suite to test instances of Functor
class FunctorTestSuite extends FunSuite {
   import Scalaz._

   test("Option Functor") {
      val xOpt:Option[Int] = Some(4)
    
      expect(Some(5)) {
         xOpt.map(1 +)
      } 

      expect(Some(6)) {
         xOpt.map(1 +).map(1 +)
      }
   }

   // Test Either as a functor.
   // map will apply the function when the instance of the Either instance is a Right.
   // Otherwise, it keeps the instance as it is.
   test("Either Functor") {

      val xEith:Either[String,Int] = Right(6)
      
      expect(Right(7)) {
         xEith.map(1 +)
      }

      val yEith:Either[String,Int] = Left("Error")
      expect(Left("Error")) {
         yEith.map(1 +)
      }

      expect(Right(14)) {
         xEith.map(1 +).map(2 *) 
      }
 
      expect(Left("Error")) {
         yEith.map(1 +).map(2 *)
      } 
   }

   // map applied to a function is actually a function composition
   test("Function Functor") {
      val f:Int=>Int  = 1 + 
      val g:Int=>Int  = 2 * 
      val h:Int=>Int  = 3 *

      expect(36) {
         (f.map(g).map(h))(5)
      }
   }

   // test functor composition. Here, compose List with Option, and vice versa.
   test("Functor Composition") {
      val xs  = List(Some(4), Some(6), Some(5), None, Some(6))
      val FLOpt = Functor[List].compose(Functor[Option])
      
      expect(List(Some(5), Some(7), Some(6), None, Some(7))){
         FLOpt.map(xs)(1 +)
      }
 
      val FOptL = Functor[Option].compose(Functor[List])
      
      val ys:Option[List[Int]] = Some(List(1, 4, 5))

      expect(Some(List(2, 5, 6))) {
         FOptL.map(ys)(1 +)
      }
   }

  
   test("Functor Product") {
      val yEith: Either[String,Int] = Right(10)
      val zEith: Either[String,Int] = Left("Error")

      val xs = (List(11, 12, 13), yEith)
      val ys = (List(1, 5), zEith)

      type EitherString[A] = ({type L[a]=Either[String, a]})#L[A]

      val F = Functor[List].product(Functor[EitherString])
      expect( (List(12, 13, 14), Right(11)) ) {
         F.map(xs)(1 +) 
      }
  
   }


   // test functor tuple. The map is only for the last element of tuple. 
   test("Tuple Functor") {

      val incr:Int=>Int = x => x + 1
      val t1 = (1,"hello")

      expect((1, "HELLO")) {
         t1.map(_.toUpperCase)
      }

      val t2 = (1, 2, "Hi")
      expect((1,2, "HI")) {
         t2.map(_.toUpperCase)
      }

      val t3 = (1, "B", 3)
      expect((1, "B", 4)) {
         t3.map(1 +)
      }

      val t4 = (1, "B", 's, (x:Int)=> x + 2)
      expect((1, "B", 's, 9)) {
         t4.map(g => g compose incr) match {
            case (a, s, symb, f) => (a, s, symb, f(6))
         }
      }

      val t5 = (2, "C", 's, Nil, 1)
      expect((2, "C", 's, Nil, 2)) {
         t5.map(incr)
      }

      val t6 = (4, "D", "E", Nil, (x:Int)=>x+1, List(1, 3, 4))
      expect(1) {
         t6.map(_.head)._6
      }

      val t7 = (1, 0, 1, 3, 4, "Hi", 5)
      expect((1, 0, 1, 3, 4, "Hi", 10)) {
         t7.map(2 *)
      }
      
      type Tuple2Int[A] = ({type L[a]=Tuple2[Int,a]})#L[A]
      type Tuple3IntString[A] = ({type L[a]=Tuple3[Int,String,A]})#L[A]
      
      val t3Comp = (4, "Bla", Some("Hi I'm t3"))
      val F = Functor[Tuple3IntString].compose(Functor[Option]) 
      expect(4, "Bla", Some("HI I'M T3")) {
         F.map(t3Comp)(_.toUpperCase)
      }     
   }

   test("Stream Functor") {
       expect(List(5, 6, 7)) {
          Functor[Stream].map(Stream.from(4))(1 +).take(3).toList
       }
   }

   test("StrengthL and StrengthR") {
      val xOpt: Option[Int] = Some(4)
      val xList = List(2, 4, 5)
      val xEith:Either[String, Int] = Right(4)
      val yEith:Either[String, Int] = Left("Error")
  
      expect(Some("Hello", 4)) {
         xOpt.strengthL("Hello")
      }

      expect(Some(4, "Hello")) {
         xOpt.strengthR("Hello")
      }


      expect(List( ("Hi", 2), ("Hi", 4), ("Hi", 5))) {
        xList.strengthL("Hi")
      }

      expect(Right(4, "A")) {
        xEith.strengthR("A")
      }

      expect(Left("Error")) {
        yEith.strengthL("A")
      }
   }
}

