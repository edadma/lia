package funl.lia

import java.{lang => jl}

import math.{sqrt => sqr, pow => po, _}

import org.scalatest._
import prop.PropertyChecks

import Util._


class TestLIA extends FreeSpec with PropertyChecks with Matchers with Assertions
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

	"ComplexDouble" in
	{
		import Complex._

		(3 + 4.i) + (5 + 6.i) shouldBe 8 + 10.i
		(3 + 4.i) - (5 + 6.i) shouldBe -2 - 2.i
		(3 + 4.i).abs shouldBe 5
		(3 + 4.i)*(5 + 6.i) shouldBe -9 + 38.i
		(-9 + 38.i)/(5 + 6.i) shouldBe 3 + 4.i
		(3 + 4.i)*(3 + 4.i).inv shouldBe 1
		((3 + 4.i) match {case Complex( a, b ) => (a, b)}) shouldBe (3, 4)
		(3 + 4.i).sqrt shouldBe (2 + 1.i)
		(5 + 0.i).sqrt shouldBe sqr(5)
		(-1 + 0.i).sqrt shouldBe (1.i)
		(-4 + 0.i).sqrt shouldBe (2.i)
		(Pi.i).exp + 1 shouldBe 0
		(1 + Pi.i).exp shouldBe -E
		(1.i)^(1.i) shouldBe exp(-Pi/2)
		(3 + 4.i)^0.5 shouldBe (3 + 4.i).sqrt
		(3 + 0.i).cos shouldBe cos(3)
		(3 + 0.i).cosh shouldBe cosh(3)
		(.5 + 0.i).acos shouldBe acos(.5)
		(3 + 0.i).acosh shouldBe acosh(3)
		(3 + 0.i).sin shouldBe sin(3)
		(3 + 0.i).sinh shouldBe sinh(3)
		(.5 + 0.i).asin shouldBe asin(.5)
		(3 + 0.i).asinh shouldBe asinh(3)
		(3 + 0.i).tan shouldBe tan(3)
		(.5 + 0.i).atan shouldBe atan(.5)
		(.5 + 0.i).atanh shouldBe atanh(.5)
		(3 + 0.i).tanh shouldBe tanh(3)
		assert( (3 + 4.i).asin.sin roughly (3 + 4.i) )
		assert( (3 + 4.i).acos.cos roughly (3 + 4.i) )
		assert( (3 + 4.i).atan.tan roughly (3 + 4.i) )
		assert( (3 + 4.i).asinh.sinh roughly (3 + 4.i) )
		assert( (3 + 4.i).acosh.cosh roughly (3 + 4.i) )
		assert( (3 + 4.i).atanh.tanh roughly (3 + 4.i) )
	}
	
	"Rational" in
	{
		import Rational._
	
		(3 over 4 match {case Rational( a, b ) => (a, b)}) shouldBe (3, 4)
		3 over 4 shouldBe Rational( 3, 4 )
		(3 over 4) ^ 5 shouldBe (243 over 1024)
		(3 over 4) ^ -5 shouldBe (1024 over 243)
		(0 over 1) ^ 0 shouldBe 0
		(3 over 4) ^ 1 shouldBe (3 over 4)
		
// 		pow( BigInt(3), BigInt(0) ) shouldBe 1
// 		pow( BigInt(0), BigInt(3) ) shouldBe 0
	}
	
// 	"Utils" in
// 	{
// 		(Math.sqrt( BigDecimal(3) )*Math.sqrt( BigDecimal(3) )).toDouble shouldBe 3
// 		Math.Pi.v.toDouble shouldBe Pi
// 		Math.E.v.toDouble shouldBe E
// 		Math.exp( 0 ) shouldBe 1
// 		Math.exp( -1 ).toDouble shouldBe 1/E
// 		Math.exp( 1 ).toDouble shouldBe E
// 		Math.exp( 100 ).toDouble shouldBe exp( 100 )
// 		Math.exp(Math.ln( 1.1 )).toDouble shouldBe 1.1
// 		nearly( Math.ln( 1 ).toDouble, 0 ) shouldBe true
// 		Math.ln( Math.E.v ).toDouble shouldBe 1
// 		Math.ln( Math.E.v*Math.E.v ).toDouble shouldBe 2
// 		Math.ln( Math.exp(5) ).toDouble shouldBe 5
// 		Math.sin( 0 ) shouldBe 0
// 		Math.sin( Math.Pi.v/2 ).toDouble shouldBe 1
// 		nearly( Math.sin( Math.Pi.v ).toDouble, 0 ) shouldBe true
// 		Math.sin( 1.5*Math.Pi.v ).toDouble shouldBe -1
// 		nearly( Math.sin( 2*Math.Pi.v ).toDouble, 0 ) shouldBe true
// 		Math.cos( 0 ) shouldBe 1
// 		nearly( Math.cos( Math.Pi.v/2 ).toDouble, 0 ) shouldBe true
// 		nearly( Math.cos( Math.Pi.v ).toDouble, -1 ) shouldBe true
// 		nearly( Math.cos( 1.5*Math.Pi.v ).toDouble, 0 ) shouldBe true
// 		nearly( Math.cos( 2*Math.Pi.v ).toDouble, 1 ) shouldBe true
// 		Math.acos( 0 ).toDouble shouldBe Pi/2
// 		Math.acos( 1 ).toDouble shouldBe 0
// 		Math.acos( -1 ).toDouble shouldBe Pi
// 		Math.acos( .5 ).toDouble shouldBe acos( .5 )
// 		Math.atan( .5 ).toDouble shouldBe atan( .5 )
// 		Math.atan( 1 ).toDouble shouldBe Pi/4
// 		Math.atan( 0 ).toDouble shouldBe 0
// 		Math.atan( -1 ).toDouble shouldBe -Pi/4
// 		Math.atan2( 0, 0).toDouble shouldBe 0
// 		Math.atan2( 1, 0).toDouble shouldBe Pi/2
// 		Math.atan2( 0, 1).toDouble shouldBe 0
// 		Math.atan2( 1, 1).toDouble shouldBe Pi/4
// 		Math.atan2( -1, 0).toDouble shouldBe -Pi/2
// 		Math.atan2( 0, -1).toDouble shouldBe Pi
// 		Math.atan2( -1, -1).toDouble shouldBe -.75*Pi
// 		Math.atan2( -1, 1).toDouble shouldBe -Pi/4
// 		Math.atan2( 1, -1).toDouble shouldBe .75*Pi
// 		Math.atanh( .5 ).round( java.math.MathContext.DECIMAL64 ).toDouble shouldBe atanh( .5 )
// 		pow( BigInt(3), BigInt(5) ) shouldBe 243
// 		pow( BigInt(0), BigInt(0) ) shouldBe 0
// 		pow( BigInt(3), BigInt(1) ) shouldBe 3
// 		pow( BigInt(3), BigInt(0) ) shouldBe 1
// 		pow( BigInt(0), BigInt(3) ) shouldBe 0
// 	}

	"Math (LIA)" in
	{
		import Rational._
		import Complex._

		Math( '+, 1.5, 2 ) should (be (3.5) and be (a [jl.Double]))
		Math( '+, BigDecimal(1.5), 2 ) should (be (3.5) and be (a [BigDecimal]))
		Math( '+, BigInt(1), 2 ) should (be (3) and be (a [jl.Integer]))
		Math( '+, BigInt(1), BigInt(2) ) should (be (3) and be (a [jl.Integer]))
		Math( '+, BigInt(1), 2.5 ) should (be (3.5) and be (a [BigDecimal]))
		Math( '^, -1.0, .5 ) shouldBe 1.i
		Math( '^, 3, -4 ) shouldBe (1 over 81)
		Math( '^, BigInt(3), -4 ) shouldBe (1 over 81)
		Math( '^, 3, BigInt(-4) ) shouldBe (1 over 81)
		
// 		Math( 'sqrt, 3 ) shouldBe sqr( 3 )
// 		Math( 'sqrt, 4 ) should (be (2) and be (a [jl.Integer]))
// 		Math( 'sqrt, -4 ) shouldBe 2.i
// // 		Math( 'sqrt, 3L ) shouldBe Math.sqrt( 3 )
// // 		Math( 'sqrt, 4L ) should (be (2) and be (a [jl.Integer]))
// 		Math( 'sqrt, BigDecimal(3) ) shouldBe Math.sqrt( 3 )
// 		Math( 'sqrt, BigInt(4) ) should (be (2) and be (a [jl.Integer]))
// 		Math( 'sqrt, BigInt(Long.MaxValue)*2*BigInt(Long.MaxValue)*2 ) should (be (BigInt(Long.MaxValue)*2) and be (a [BigInt]))
// 		Math( 'sqrt, 3 over 4 ) shouldBe Math.sqrt( Math.bigDecimal(3)/4 )
// 		Math( 'sqrt, 4 over 3 ) shouldBe Math.sqrt( Math.bigDecimal(4)/3 )
// 		Math( 'sqrt, 4 over 9 ) shouldBe (2 over 3)
	}
}