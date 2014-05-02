package funl.lia

import math._

import Util._


class Rational( _n: BigInt, _d: BigInt ) extends Number with Ordered[Rational]
{
	import Rational._
	
	require( _d != ZERObi, "denominator can't be zero" )
	
	val (n, d) =
		{
		val g = _n gcd _d
		val sign = _d.signum
		
			(_n/g*sign, _d/g*sign)
		}
	
	lazy val isZero = n == ZERObi

	lazy val isInt = d == ONEbi
	
	def maybeDemote =
		if (isInt)
			if (n.isValidInt)
				n.toInt
			else
				n
		else
			this

	def +( that: Rational ) = Rational( n*that.d + that.n*d, d*that.d )
	
	def *( that: Rational ) = Rational( n*that.n, d*that.d )
	
	def -( that: Rational ) = Rational( n*that.d - that.n*d, d*that.d )
	
	def /( that: Rational ) = Rational( n*that.d, d*that.n )
	
	def ^( that: Int ): Rational =
	{
		def _pow( b: Rational, e: Int ): Rational =
			if (e == 1)
				b
			else if ((e&1) == 1)
				b*_pow( b*b, (e - 1)/2 )
			else
				_pow( b*b, e/2 )

		if (this == ZERO)
			ZERO
		else if (that == 0)
			ONE
		else if (that < 0)
			_pow( this.inv, -that )
		else
			_pow( this, that )
	}
	
	def ^( that: BigInt ): Rational =
	{
		def _pow( b: Rational, e: BigInt ): Rational =
			if (e == 1)
				b
			else if (e testBit 0)
				b*_pow( b*b, (e - 1)/2 )
			else
				_pow( b*b, e/2 )

		if (this == ZERO)
			ZERO
		else if (that == ZERObi)
			ONE
		else if (that < ZERObi)
			_pow( this.inv, -that )
		else
			_pow( this, that )
	}
	
	lazy val unary_- = Rational( -n, d )
	
	lazy val inv =
		if (isZero)
			sys.error( "no inverse" )
		else
			Rational( d, n )

	lazy val abs = if (n > ZERObi && d > ZERObi) this else new Rational( n.abs, d.abs )
	
	def decimalValue( m: LIA ) = m.bigDecimal( n )/m.bigDecimal( d )
	
	lazy val doubleValue = n.toDouble/d.toDouble

	lazy val floatValue = n.toFloat/d.toFloat

	lazy val intValue = (n/d).toInt

	lazy val longValue = (n/d).toLong

	def compare( that: Rational ) = (n*that.d).compare( that.n*d )
	
	override def equals( x: Any ) =
		x match
		{
			case r: Rational => n == r.n && d == r.d
			case bi: BigInt => isInt && n == bi
			case d: Double => nearly( doubleValue, d )
			case l: Long => isInt && n == l
			case i: Int => isInt && n == i
			case _ => false
		}
	
	override lazy val toString = if (isZero) "0" else if (isInt) n.toString else n + "/" + d
}

object Rational
{
	lazy val ZERO = Rational( 0 )
	lazy val ONE = Rational( 1 )
	
	def oneOver( d: BigInt ) = new Rational( ONEbi, d )
	
	def apply( n: BigInt, d: BigInt ) = new Rational( n, d )
	
	def apply( a: BigInt ) = new Rational( a, ONEbi )
	
	def apply( a: Int ) = new Rational( BigInt(a), ONEbi )
	
	def apply( a: Long ) = new Rational( BigInt(a), ONEbi )
	
	def unapply( z: Any ): Option[(BigInt, BigInt)] =
		z match
		{
			case r: Rational => Some( r.n, r.d )
			case _ => None
		}

	implicit def int2rational( a: Int ): Rational = Rational( a )
	
	implicit def bigint2rational( a: BigInt ): Rational = Rational( a )
	
	implicit def intdiv2rational( a: Int ): IntDiv = new IntDiv( a )
	
	class IntDiv( a: Int )
	{
		def over( b: Int ) = new Rational( a, b )
	}
}