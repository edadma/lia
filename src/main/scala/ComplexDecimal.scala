package funl.lia

import Util._


class ComplexDecimal( val a: BigDecimal, val b: BigDecimal, private[lia] val m: LIA ) extends Number
{
	implicit def bigDecimal2ComplexDecimal( a: BigDecimal ) = new ComplexDecimal( a, 0, m )
	implicit def int2ComplexDecimal( a: Int ) = new ComplexDecimal( m.bigDecimal(a), 0, m )
	
	lazy val sq = this*this
	
	lazy val norm = a*a + b*b
	
	lazy val abs = m.sqrt( norm )
	
	lazy val arg = m.atan2( b, a )
	
	lazy val sqrt = new ComplexDecimal( m.sqrt((abs + a)/2), b.signum*m.sqrt((abs - a)/2), m )

	lazy val ln = new ComplexDecimal( m.ln(abs), arg, m )
	
	lazy val exp = m.exp(a)*new ComplexDecimal(m.cosBigDecimal(b), m.sinBigDecimal(b), m)
	
	lazy val sin = ((this*m.I.v).exp - (-this*m.I.v).exp)/2/m.I.v
	
	lazy val asin = (-m.I.v)*((m.I.v*this + (1 - sq).sqrt).ln)
	
	lazy val sinh = (exp - (-this).exp)/2
	
	lazy val asinh = (this + (sq + 1).sqrt).ln
	
	lazy val cos = ((this*m.I.v).exp + (-this*m.I.v).exp)/2
	
	lazy val acos = m.I.v*((this - m.I.v*(1 - sq).sqrt).ln)
	
	lazy val cosh = (exp + (-this).exp)/2
	
	lazy val acosh = (this + (this + 1).sqrt*(this - 1).sqrt).ln
	
	lazy val tan =
	{
	val e = (2*m.I.v*this).exp
	
		(e - 1)/m.I.v/(e + 1)
	}
	
	lazy val atan = m.I.v*((1 - this*m.I.v).ln - (1 + this*m.I.v).ln)/2
	
	lazy val tanh = (exp - (-this).exp)/(exp + (-this).exp)
	
	lazy val atanh = ((1 + this).ln - (1 - this).ln)/2
	
	lazy val unary_- = new ComplexDecimal( -a, -b, m )
	
	lazy val conj = new ComplexDecimal( a, -b, m )
	
	lazy val inv =
		if (isZero)
			sys.error( "no inverse" )
		else
			conj/norm

	lazy val doubleValue = abs.toDouble

	lazy val floatValue = abs.toFloat

	lazy val intValue = abs.toInt

	lazy val longValue = abs.toLong

	lazy val isZero = m.nearly( a, 0 ) && m.nearly( b, 0 )

	def ^( that: ComplexDecimal ) = (that*ln).exp

	def ^( that: BigDecimal ) = (ln*that).exp

	def +( that: ComplexDecimal ) = new ComplexDecimal( a + that.a, b + that.b, m )

	def +( that: Int ) = new ComplexDecimal( a + that, b, m )
	
	def *( that: ComplexDecimal ) = new ComplexDecimal( a*that.a - b*that.b, b*that.a + a*that.b, m )
	
	def *( that: BigDecimal ) = new ComplexDecimal( a*that.a, b*that.a, m )
	
	def -( that: ComplexDecimal ) = new ComplexDecimal( a - that.a, b - that.b, m )
	
	def /( that: ComplexDecimal ) = new ComplexDecimal( (a*that.a + b*that.b)/that.norm, (b*that.a - a*that.b)/that.norm, m )

	override def equals( that: Any ) =
		that match
		{
			case ComplexDecimal( x, y ) => m.nearly( a, x ) && m.nearly( b, y )
			case Complex( x, y ) => m.nearly( a, x ) && m.nearly( b, y )
			case d: BigDecimal => m.nearly( a, d ) && m.nearlyZero( b )
			case d: Double => nearly( a.toDouble, d ) && m.nearlyZero( b )
			case i: Int => m.nearly( a, i ) && m.nearlyZero( b )
			case _ => false
		}
	
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

object ComplexDecimal
{
	def unapply( z: Any ): Option[(BigDecimal, BigDecimal)] =
		z match
		{
			case d: ComplexDecimal => Some( d.a, d.b )
			case _ => None
		}
}