package xyz.hyperreal.lia

import java.{lang => jl}

import math._
import org.scalatest._
import xyz.hyperreal.numbers.{Math => NM, _}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class TestLIA extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Assertions
{
// 	"ComplexDecimal" in
// 	{
// 		Math( 3, 4 ) + Math( 5, 6 ) shouldBe Math( 8, 10 )
// 		Math( 3, 4 ) - Math( 5, 6 ) shouldBe Math( -2, -2 )
// 		Math( 3, 4 )*Math( 5, 6 ) shouldBe Math( -9, 38 )
// 		Math( -9, 38 )/Math( 5, 6 ) shouldBe Math( 3, 4 )
// 		Math( 3, 4 ).abs shouldBe 5
// 		Math( 3, 4 )*Math( 3, 4 ).inv shouldBe 1
// 		(Math( 3, 4 ) match {case ComplexDecimal( a, b ) => (a, b)}) shouldBe (3, 4)
// 		Math( 3, 4 ).sqrt shouldBe Math( 2, 1 )
// 		(Math.I.v*Math.Pi.v).exp + 1 shouldBe 0
//  		(Math.I.v*Math.Pi.v + 1).exp shouldBe -E
//  		(Math.I.v)^(Math.I.v) shouldBe exp(-Pi/2)
//  		Math( 3, 4 )^0.5 shouldBe Math(3, 4).sqrt
//  		Math( 3, 0 ).cos shouldBe cos(3)
//  		Math( 3, 0 ).cosh shouldBe cosh(3)
// 		Math( .5, 0 ).acos shouldBe acos(.5)
//  		Math( 3, 0 ).acosh shouldBe acosh(3)
// 		Math( 3, 0 ).sin shouldBe sin(3)
//  		Math( 3, 0 ).sinh.doubleValue shouldBe sinh(3)
//  		Math( .5, 0 ).asin shouldBe asin(.5)
//  		Math( 3, 0 ).asinh shouldBe asinh(3)
//  		Math( 3, 0 ).tan shouldBe tan(3)
//  		Math( .5, 0 ).atan shouldBe atan(.5)
//  		Math( .5, 0 ).atanh shouldBe atanh(.5)
//  		Math( 3, 0 ).tanh shouldBe tanh(3)
//  		Math(3, 4).asin.sin shouldBe Math(3, 4)
//  		Math(3, 4).acos.cos shouldBe Math(3, 4)
//  		Math(3, 4).atan.tan shouldBe Math(3, 4)
//  		Math(3, 4).asinh.sinh shouldBe Math(3, 4)
//  		Math(3, 4).acosh.cosh shouldBe Math(3, 4)
//  		Math(3, 4).atanh.tanh shouldBe Math(3, 4)
// 	}

//	"Math operations" in
//	{
//		import Rational._
//		import ComplexDouble._
//
//		Math( Symbol("+"), 1.5, 2 ) should (be (3.5) and be (a [jl.Double]))
//		Math( Symbol("+"), BigDecimal(1.5), 2 ) should (be (3.5) and be (a [BigDecimal]))
//		Math( Symbol("+"), BigInt(1), 2 ) should (be (3) and be (a [jl.Integer]))
//		Math( Symbol("+"), BigInt(1), BigInt(2) ) should (be (3) and be (a [jl.Integer]))
//		Math( Symbol("+"), BigInt(1), 2.5 ) shouldBe 3.5
//		Math( '^, -1.0, .5 ) shouldBe 1.i
//		Math( '^, 3, -4 ) shouldBe (1\81)
//		Math( '^, BigInt(3), -4 ) shouldBe (1\81)
//		Math( '^, 3, BigInt(-4) ) shouldBe (1\81)
//		Math( '%, BigInt(12), 5 ) should (be (2) and be (a [jl.Integer]))
//		Math( '%, 12, 5 ) should (be (2) and be (a [jl.Integer]))
//		Math( '|, BigInt(5), 10 ) shouldBe true
//		Math( '|, 5, 10 ) shouldBe true
//		Math( '|, BigInt(5), 11 ) shouldBe false
//		Math( '|, 5, 11 ) shouldBe false
//		Math( '/|, BigInt(5), 10 ) shouldBe false
//		Math( '/|, 5, 10 ) shouldBe false
//		Math( '/|, BigInt(5), 11 ) shouldBe true
//		Math( '/|, 5, 11 ) shouldBe true
//	}

	"Math ComplexBigInt" in
	{
//		import Rational._
		import ComplexBigInt._

		Math.apply( Symbol("+"), i + 1, 2 ) shouldBe 3 + i
//		Math.apply( Symbol("+"), 1 + i, BigInt(2) ) shouldBe 3 + i
//		Math.apply( Symbol("+"), 1 + i, 1\2 ) shouldBe ComplexRational( 3\2, 1 )
//		Math.apply( Symbol("+"), 1 + i, 1.2 ) shouldBe ComplexDouble( 2.2, 1 )
//		Math.apply( Symbol("+"), 1 + i, ComplexDouble(2, 3) ) shouldBe ComplexDouble( 3, 4 )
//		Math.sqrtFunction( -4 ) should (be (2*i) and be (a [ComplexBigInt]))
	}
	
//  "Math ComplexRational" in
//  {
//		import Rational._
//		import ComplexRational._
//
//		Math.sqrtFunction( 3 ) shouldBe sqrt( 3 )
//		Math.sqrtFunction( 4 ) should (be (2) and be (a [jl.Integer]))
//		Math.sqrtFunction( -4\9 ) should (be (2\3*i) and be (a [ComplexRational]))
//		Math.sqrtFunction( BigInt(4) ) should (be (2) and be (a [jl.Integer]))
//		Math.sqrtFunction( BigInt("100000000000000000000") ) shouldBe BigInt("10000000000")
//    Math.apply( 'sqrt, BigDecimal(3) ) shouldBe Math.sqrt( 3 )
//    Math.apply( 'sqrt, BigInt(4) ) should (be (2) and be (a [jl.Integer]))
//    Math.apply( 'sqrt, BigInt(Long.MaxValue)*2*BigInt(Long.MaxValue)*2 ) should (be (BigInt(Long.MaxValue)*2) and be (a [BigInt]))
//    Math.apply( 'sqrt, 3/4 ) shouldBe Math.sqrt( Math.bigDecimal(3)/4 )
//    Math.apply( 'sqrt, 4/3 ) shouldBe Math.sqrt( Math.bigDecimal(4)/3 )
//    Math.apply( 'sqrt, 4/9 ) shouldBe (2/3)
//  }
}
