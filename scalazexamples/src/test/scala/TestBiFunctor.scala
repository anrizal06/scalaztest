package org.riv.scalaztest

import org.scalatest.FunSuite
import scalaz._

// Some examples of BiFunctor
class BiFunctorTestSuite extends FunSuite {

   import Scalaz._

   test("BiFunctor Either") {
      
      val xEith:Either[String,Int] = Right(6)

      // incr is applied to Right, and upper to Left.
      val upper = (s:String)=>s.toUpperCase
      val incr = (x:Int) => x + 1

      expect(Right(7)) {
        xEith.bimap(upper, incr)
      }

      val yEith:Either[String,Int] = Left("boom")
      expect(Left("BOOM")) {
        yEith.bimap(upper,incr)
      }

      val BF = BiFunctor[Either] 
      expect(Left("BOOM")) {
         BF.leftFunctor.map(yEith)(upper)
      }

   }

   test("BiFunctor Tuple") {
      val x = (1, "Help")
 
      val upper = (s:String) => s.toUpperCase
      val incr = (x:Int) => x + 1
      val len = (s:String) => s.length

      // incr is applied to the first element, and upper is applied to the second element
      expect((2, "HELP")) {
         x.bimap(incr, upper)
      }

      expect((3, 4)) {
         x.bimap(incr, upper).bimap(incr, len)
      }
      expect((3, "HELP")) {
         x.bimap(incr, upper).bimap(incr, upper)
      }

   }

   // Test the obtention of a functor from a bifunctor.
   test("BiFunctor leftFunctor") {
      val BF = BiFunctor[Either]
      val yEith:Either[String,Int] = Left("boom")
      val upper = (s:String) => s.toUpperCase
      expect(Left("BOOM")) {
         BF.leftFunctor.map(yEith)(upper)
      }
   }
   test("BiFunctor rightFunctor") {
     val BF = BiFunctor[Tuple2] 
     val t = (5, "hey dawg")
     
     val upper = (s:String) => s.toUpperCase
     expect( (5,"HEY DAWG") ) {
        BF.rightFunctor.map(t)(upper)
     }
 
   }

   test("BiFunctor LeftMap RightMap") {
     val BF = BiFunctor[Tuple2]

     val t = (5, "hey dawg")

     val upper = (s:String) => s.toUpperCase
     val incr:Int=>Int  = 1 + 

     expect((5,"HEY DAWG")) {
        BF.rightMap(t)(upper)
     }

     expect((6,"hey dawg")) {
        BF.leftMap(t)(incr)
     }

   }

   // Uniformly map each element of a tuple. 
   test("BiFunctor umap") {
      val BF = BiFunctor[Tuple2]

      val t = (5, 6)

      val incr:Int=>Int = 1 +
      
      expect((6,7)) {
         BF.umap(t)(incr)
      }
      
      // Note that, BF.umap((6,"7")) does not compile.
   }
}
