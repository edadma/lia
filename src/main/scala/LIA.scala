/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.lia

import java.math.{RoundingMode, MathContext}
import java.{lang => jl}

import collection.mutable.HashMap
import math.{log => lg, _}

import Util._


class LIA extends ((Symbol, Any*) => AnyRef)
{
	implicit def int2bigDecimal( a: Int ) = bigDecimal( a )
	implicit def double2bigDecimal( a: Double ): BigDecimal = bigDecimal( a )
	implicit def long2bigDecimal( a: Long ) = bigDecimal( a )
//	implicit def int2complexDecimal( a: Int ) = complexDecimal( bigDecimal(a), 0 )
	
	def apply( a: BigDecimal, b: BigDecimal ) = complexDecimal( a, b )
	
	class Const( compute: => BigDecimal )
	{
		private var _value: BigDecimal = null
		
		def v =
		{
			if (_value == null || _value.mc != mc)
				_value = compute
				
			_value
		}
	}
	
	class ComplexConst( compute: => ComplexDecimal )
	{
		private var _value: ComplexDecimal = null
		
		def v =
		{
			if (_value == null || _value.m.mc != mc)
				_value = compute
				
			_value
		}
	}
	
	class IntConst( a: Int ) extends Const( bigDecimal(a) )
	
	class DoubleConst( a: Double ) extends Const( bigDecimal(a) )
	
	protected val ZERO = new IntConst( 0 )
	protected val QUARTER = new DoubleConst( .25 )
	protected val ONE = new IntConst( 1 )
	protected val TWO = new IntConst( 2 )
	protected val THREE = new IntConst( 3 )
	protected val FOUR = new IntConst( 4 )
	
	val Pi = new Const( compute_pi )
	val E = new Const( compute_e )
	val LN2 = new Const( compute_ln2 )
	val LN2D = lg( 2 )
	val I = new ComplexConst( complexDecimal(0, 1) )
	
	protected var mc = MathContext.DECIMAL128
	
	protected val operations = new HashMap[Symbol, Map[Seq[Class[_]], Seq[AnyRef] => AnyRef]]
	
	protected val specials = new HashMap[(String, String), String]
	
	protected def special( s: ((String, String), String) )
	{
		specials(s._1) = s._2
	}
	
	def ln( x: BigDecimal ) =
	{
	val p = mc.getPrecision*lg( 10 )/LN2D
	val m = ceil( p/2 - lg( x.toDouble )/LN2D ).toInt
	val s = x*TWO.v.pow( m )
	
		Pi.v/(TWO.v*agm( ONE.v, FOUR.v/s )) - m*LN2.v
	}

	def agm( x: BigDecimal, y: BigDecimal ) =
	{
		def am( a: BigDecimal, b: BigDecimal ) = (a + b)/2
		
		def gm( a: BigDecimal, b: BigDecimal ) = sqrt( a*b )
	
		def recur( an: BigDecimal, gn: BigDecimal ): BigDecimal =
		{
		val anp1 = am( an, gn )
		val gnp1 = gm( an, gn )
		
			if ((anp1 - gnp1).abs <= an.ulp)
				anp1
			else
				recur( anp1, gnp1 )
		}
		
		recur( am(x, y), gm(x, y) )
	}

	def sqrt( x: BigDecimal ) =
	{
	var new_guess = x/TWO.v
	var current_guess = x
	
		while (current_guess != new_guess)
		{
			current_guess = new_guess
			new_guess = (current_guess + x/current_guess)/TWO.v
		}
		
		new_guess
	}
	
	def inv( x: BigDecimal ) = ONE.v/x
	
	def xx( x: BigDecimal ) = x*x
	
	def compute_ln2 =
	{
	var res = ZERO.v
	var p3 = THREE.v
	var p4 = FOUR.v
	var term = 1.0/p3 + 1.0/p4
	var k = 1
	
		while (term.scale < term.precision*2)
		{
			res += term
			p3 *= 3
			p4 *= 4
			k += 1
			term = (1.0/p3 + 1.0/p4)/k
		}
		
		res.round( mc )
	}
	
	def compute_pi =
	{
	var a = ONE.v
	var b = inv( sqrt(TWO.v) )
	var t = QUARTER.v
	var x = 1
	
		while (a != b)
		{
		val y = a
		
			a = (a + b)/TWO.v
			b = sqrt( b*y )
			t = t - BigDecimal(x)*xx(y - a)
			x <<= 1
		}
		
		xx( a + b )/(FOUR.v*t)
	}
	
	def compute_e =
	{
	var result = ZERO.v
	var term = TWO.v
	var d = ONE.v
	var i = TWO.v
	
		while (term.scale < term.precision*2)
		{
			result += term
			d *= i
			i += 1
			term = inv( d )
		}
		
		result.round( mc )
	}
	
	def exp( a: BigDecimal ) =
	{
	val x_ = a
	var result = x_ + ONE.v
	var n = x_
	var d = ONE.v
	var term = x_
	var i = 2
	
		while (term.scale < term.precision*2)
		{
			n *= x_
			d *= bigDecimal( i )
			term = n/d
			result += term
			i += 1
		}
		
		result.round( mc )
	}
	
// 	def ln( a: BigDecimal ) =
// 	{
// 	var result = ZERO.v
// 	val xm1 = a - ONE.v
// 	val xp1 = a + ONE.v
// 	val xm2 = xx( xm1 )
// 	val xp2 = xx( xp1 )
// 	var n = xm1
// 	var d = xp1
// 	var term = xm1/xp1
// 	var i = 3
// 	
// 		while (term.scale < term.precision*2)
// 		{
// 			result += term
// 			n *= xm2
// 			d *= xp2
// 			term = n/(d*i)
// 			i += 2
// 		}
// 		
// 		result*TWO.v
// 	}

	def log( b: BigDecimal, x: BigDecimal ) = ln(x)/ln(b)
	
	def pow( x: BigDecimal, y: BigDecimal ) = exp( y*ln(x) )
	
	def pow( x: BigDecimal, y: Double ) = exp( bigDecimal(y)*ln(x) )
	
	def sin( a: BigDecimal ) =
	{
	var term = a
	val x2 = xx( a )
	var n = term
	var d = BigInt( 1 )
	var result = ZERO.v
	var i = 3
	
		while (term.scale < term.precision*2)
		{
			if ((i&2) == 0)
				result -= term
			else
				result += term
				
			n *= x2
			d *= BigInt( (i-1)*i )
			term = n/BigDecimal( d )
			i += 2
		}
		
//		if (result.compareTo( ONE ) > 0)
//			return ONE;
//		else if (result.compareTo( NEG_ONE ) < 0)
//			return NEG_ONE;
		
		result.round( mc )
	}
	
	def cos( a: BigDecimal ) =
	{
	var term = ONE.v
	val x2 = xx( a )
	var n = term
	var d = BigInt( 1 )
	var result = ZERO.v
	var i = 2
	
		while (term.scale < term.precision*2)
		{
			if ((i&2) == 0)
				result -= term
			else
				result += term
				
			n *= x2
			d *= BigInt( (i-1)*i )
			term = n/BigDecimal( d )
			i += 2
		}
	
//		if (result.compareTo( ONE ) > 0)
//			return ONE;
//		else if (result.compareTo( NEG_ONE ) < 0)
//			return NEG_ONE;
	
		result.round( mc )
	}

	def acos( a: BigDecimal ) =
	{
	var a_ = ZERO.v
	var x1 = a
	var halves = ONE.v

		require( a.abs <= ONE.v, "acos() argument may not exceed one" )
		
		while ({halves /= TWO.v; halves.scale < halves.precision*2})
			if (x1.signum < 0)
			{
				x1 = ONE.v - TWO.v*xx( x1 )
				a_ += halves
			}
			else
				x1 = TWO.v*xx(x1) - ONE.v
		
		(Pi.v*a_).round( mc )
	}

	def atan( a: BigDecimal ) = a.signum*acos( inv(sqrt(xx(a) + ONE.v)) )

	def atan2( y: BigDecimal, x: BigDecimal ) =
		if (x > 0)
			atan( y/x )
		else if (y >= 0 && x < 0)
			atan( y/x ) + Pi.v
		else if (y < 0 && x < 0)
			atan( y/x ) - Pi.v
		else if (y > 0 && x == 0)
			Pi.v/2
		else if (y < 0 && x == 0)
			-Pi.v/2
		else
			bigDecimal( 0 )
	
	def atanh( x: BigDecimal ) = (ln(ONE.v + x) - ln(ONE.v - x))/TWO.v
	
	def bisqrt( n: BigInt ) =
	{
	val dr = sqrt( bigDecimal(n) )
	val ir = dr.toBigInt

		if (ir*ir == n)
			Left( ir )
		else
			Right( dr )
	}

	def complexDecimal( a: BigDecimal, b: BigDecimal ) = new ComplexDecimal( a, b, this )
	
	def bigDecimal( n: Long ): BigDecimal = BigDecimal( n, mc )

	def bigDecimal( n: Int ): BigDecimal = BigDecimal( n, mc )
	
	def bigDecimal( n: Double ): BigDecimal = BigDecimal( n, mc )

	def bigDecimal( r: Rational ): BigDecimal =
	{
	val quo = bigDecimal( r.n )/bigDecimal( r.d )

	  if (quo.precision > mc.getPrecision) quo.round( mc ) else quo
	}

	def bigDecimal( n: BigInt ): BigDecimal =
	{
	val len = digits( n )
	val m = if (len >= mc.getPrecision) mathContext(len + 5) else mc

	  BigDecimal( n, m )
	}
	
	def decimal( s: String ) =
	{
	val res = BigDecimal( s, mc )
	
		if (res.isExactDouble)//isValidDouble)
			res.toDouble
		else
			res
	}
	
	def toBigDecimal( a: Number ): BigDecimal =
		a match
		{
			case bd: BigDecimal => bd
			case i: jl.Integer => bigDecimal( i )
			case d: jl.Double => bigDecimal( d )
//			case l: jl.Long => bigDecimal( l )
			case r: Rational => bigDecimal( r )
			case bi: BigInt => bigDecimal( bi )
			case _ => sys.error( "can't convert from " + a )
		}
	
	def toComplexDouble( a: Number ): ComplexDouble =
		a match
		{
			case cd: ComplexDouble => cd
			case i: jl.Integer => Complex( i )
			case d: jl.Double => Complex( d )
//			case l: jl.Long => Complex( l )
			case _ => sys.error( "can't convert from " + a )
		}
	
	def toComplexDecimal( a: Number ): ComplexDecimal =
		a match
		{
			case cd: ComplexDecimal => cd
			case bd: BigDecimal => new ComplexDecimal( bd, 0, this )
			case i: jl.Integer => new ComplexDecimal( i.asInstanceOf[Int], 0, this )
			case d: jl.Double => new ComplexDecimal( d.asInstanceOf[Double], 0, this )
//			case l: jl.Long => new ComplexDecimal( l.asInstanceOf[Long], 0, this )
			case Rational( n, d ) =>
				val quo = toBigDecimal( n )/toBigDecimal( d )
				
				new ComplexDecimal( if (quo.precision > mc.getPrecision) quo.round( mc ) else quo, 0, this )
			case bi: BigInt =>
				val len = digits( bi )
				val m = if (len >= mc.getPrecision) mathContext(len + 5) else mc

				new ComplexDecimal( BigDecimal(bi, m), 0, this )
			case _ => sys.error( "can't convert from " + a )
		}
	
	def toBigInt( a: Number ): BigInt =
		a match
		{
			case bi: BigInt => bi
			case i: jl.Integer => BigInt( i )
//			case l: jl.Long => BigInt( l )
			case _ => sys.error( "can't convert from " + a )
		}
	
	def toRational( a: Number ) =
		a match
		{
			case r: Rational => r
			case bi: BigInt => Rational( bi )
			case i: jl.Integer => Rational( i )
//			case l: jl.Long => Rational( l )
			case _ => sys.error( "can't convert from " + a )
		}

	def maybeDemote( n: BigInt ): Number =
	  if (n.isValidInt)
	    n.toInt.asInstanceOf[Number]
	  else
	    n

// 	def maybeDemote( n: Long ): Number =
// 	  if (n >= Int.MinValue && n <= Int.MaxValue)
// 	    n.toInt.asInstanceOf[Number]
// 	  else
// 	    n.asInstanceOf[Number]

	def maybePromote( n: Long ) =
		if (n >= Int.MinValue && n <= Int.MaxValue)
			n.toInt.asInstanceOf[Number]
		else
			BigInt( n )

/* 	def maybeDemote( n: Rational ): Number =
	  if (n.isValidDouble)
	    n.toDouble.asInstanceOf[Number]
	  else
	    n
 */
	protected def binary[A, B]( args: Seq[AnyRef], f: (A, B) => Any ) = f( args(0).asInstanceOf[A], args(1).asInstanceOf[B] ).asInstanceOf[AnyRef]
	
	protected def unary[A]( args: Seq[AnyRef], f: A => Any ) = f( args(0).asInstanceOf[A] ).asInstanceOf[AnyRef]
	
	protected def identity[A]( args: Seq[AnyRef] ) = args(0).asInstanceOf[A].asInstanceOf[AnyRef]

	protected def binary[N <: Number]( types: (String, (N, N) => Any)* ): Seq[(List[String], Seq[AnyRef] => AnyRef)] =
	{
	val ts = types map (_._1) zipWithIndex
	val fs = types map (_._2)
	
		def index( i: (String, Int), j: (String, Int) ) =
			specials.get( (i._1, j._1) ) match
			{
				case None => i._2 max j._2
				case Some( t ) => ts.find( _._1 == t ).get._2
			}
		
		for (i <- ts; j <- ts)
			yield
				List( i._1, j._1 ) -> (binary( (_: Seq[AnyRef]), fs(index(i, j)) ))
	}

	protected def unary[N <: Number]( types: (String, N => Any)* ): Seq[(List[String], Seq[AnyRef] => AnyRef)] =
		for ((t, f) <- types)
			yield
				List( t ) -> (unary( (_: Seq[AnyRef]), f ))

	protected def operation( sym: Symbol, maps: Seq[(List[String], Seq[AnyRef] => AnyRef)] ) =
	{
		def classForName( name: String ) =
			try
			{
				Class.forName( name )
			}
			catch
			{
				case _: Exception =>
					try
					{
						Class.forName( "java.lang." + name )
					}
					catch
					{
						case _: Exception =>
							try
							{
								Class.forName( "scala.math." + name )
							}
							catch
							{
								case _: Exception => sys.error( "number type unknown: " + name )
							}
					}
			}

		operations(sym) = Map( maps map (op => (op._1 map (classForName(_)), op._2)): _* )
	}
	
	def round( a: BigDecimal ) = a.setScale( 0, BigDecimal.RoundingMode.HALF_EVEN )
	
	def nearly( a: BigDecimal, b: BigDecimal ) = (a - b).abs < 1e-30
	
	def nearlyZero( a: BigDecimal ) = a.abs < 1e-30
	
	protected def mathContext( p: Int ) = new MathContext( p, RoundingMode.HALF_EVEN )
	
	def precision( p: Int )
	{
		require( p > 0, "precision is positive" )
		mc = mathContext( p )
	}
	
	def apply( operation: Symbol, operands: Any* ) =
	{
	val refs = operands map (_.asInstanceOf[AnyRef])

		operations( operation )( refs map (_.getClass) )( refs )
	}
}