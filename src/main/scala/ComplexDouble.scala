package ca.hyperreal.lia

import math.{sqrt => sqr, exp => ex, abs => ab, cos => co, sin => si, _}

import Util._
import Complex._


class ComplexDouble( val a: Double, val b: Double ) extends Number
{
	lazy val I = new ComplexDouble( 0, 1 )
	
	lazy val Ix = I*this
	
	lazy val nIx = -I*this
	
	lazy val sq = this*this
	
	lazy val norm = a*a + b*b
	
	lazy val abs = sqr( norm )
	
	lazy val arg = atan2( b, a )
	
	lazy val sqrt = new ComplexDouble( sqr((abs + a)/2), (if (b < 0) -1 else 1)*sqr((abs - a)/2) )

	lazy val ln = new ComplexDouble( log(abs), arg )
	
	lazy val exp = ex(a)*(new ComplexDouble(co(b), si(b)))
	
	lazy val sin = (Ix.exp - nIx.exp)/2/I
	
	lazy val asin = (-I)*((Ix + (1 - sq).sqrt).ln)
	
	lazy val sinh = (exp - (-this).exp)/2
	
	lazy val asinh = (this + (sq + 1).sqrt).ln
	
	lazy val cos = ((Ix).exp + (nIx).exp)/2
	
	lazy val acos = I*((this - I*(1 - sq).sqrt).ln)
	
	lazy val acosh = (this + (this + 1).sqrt*(this - 1).sqrt).ln
	
	lazy val cosh = (exp + (-this).exp)/2
	
	lazy val tan =
	{
	val e = (2*Ix).exp
	
		(e - 1)/I/(e + 1)
	}
	
	lazy val atan = I*((1 - Ix).ln - (1 + Ix).ln)/2
	
	lazy val tanh = (exp - (-this).exp)/(exp + (-this).exp)
	
	lazy val atanh = ((1 + this).ln - (1 - this).ln)/2
	
	lazy val unary_- = new ComplexDouble( -a, -b )
	
	lazy val conj = new ComplexDouble( a, -b )
	
	lazy val inv =
		if (isZero)
			throw new Exception( "no inverse" )
		else
			conj/norm

	lazy val doubleValue = abs

	lazy val floatValue = abs.toFloat

	lazy val intValue = abs.toInt

	lazy val longValue = abs.toLong

	lazy val isZero = nearly( a, 0 ) && nearly( b, 0 )

	def ^( that: ComplexDouble ) = (that*ln).exp

	def +( that: ComplexDouble ) = new ComplexDouble( a + that.a, b + that.b )
	
	def *( that: ComplexDouble ) = new ComplexDouble( a*that.a - b*that.b, b*that.a + a*that.b )
	
	def -( that: ComplexDouble ) = new ComplexDouble( a - that.a, b - that.b )
	
	def /( that: ComplexDouble ) = new ComplexDouble( (a*that.a + b*that.b)/that.norm, (b*that.a - a*that.b)/that.norm )

	override def equals( that: Any ) =
		that match
		{
			case c: ComplexDouble => nearly( a, c.a ) && nearly( b, c.b )
			case d: Double => nearly( a, d ) && nearly( b, 0 )
			case i: Int => nearly( a, i ) && nearly( b, 0 )
			case _ => false
		}
	
	def roughly( that: ComplexDouble ): Boolean = Util.roughly( a, that.a ) && Util.roughly( b, that.b )
	
	override lazy val hashCode = a.hashCode ^ b.hashCode
	
	override lazy val toString =
		if (b == 0)
			a.toString
		else if (a == 0)
		{
			if (b == 1)
				"i"
			else if (b == -1)
				"-i"
			else
				b.toString + "i"
		}
		else if (b == 1)
			a + "+i"
		else if (b == -1)
			a + "-i"
		else if (b < 0)
			a.toString + b + "i"
		else
			a + "+" + b + "i"
}