package xyz.hyperreal.lia

import java.math.{RoundingMode, MathContext}
import java.{lang => boxed}

import collection.mutable.HashMap

import xyz.hyperreal.numbers._

import Util._


class LIA( implicit var bdmath: BigDecimalMath )
{
	protected val operations = new HashMap[Symbol, FunctionMap]
	
	protected val specials = new HashMap[(String, String), String]
	
	protected def special( s: ((String, String), String) ) = {
		specials(s._1) = s._2
		specials((s._1._2, s._1._1)) = s._2
	}
	
	def bisqrt( n: BigInt ) =
	{
	val dr = BigDecimalMath.sqrt( bigDecimal(n) )
	val ir = dr.toBigInt

		if (ir*ir == n)
			Left( ir )
		else
			Right( dr )
	}

	def bigDecimal( n: Int ): BigDecimal = BigDecimal( n, bdmath.mc )
	
	def bigDecimal( n: Double ): BigDecimal = BigDecimal( n, bdmath.mc )

	def bigDecimal( r: Rational ): BigDecimal = r.decimalValue( bdmath.mc )
// 	{
// 	val quo = bigDecimal( r.n )/bigDecimal( r.d )
// 
// 	  if (quo.precision > mc.getPrecision) quo.round( mc ) else quo
// 	}

	def bigDecimal( n: BigInt ): BigDecimal =
	{
	val len = digits( n )
	val m = if (len >= bdmath.mc.getPrecision) mathContext(len + 5) else bdmath.mc

	  BigDecimal( n, m )
	}
	
	def decimal( s: String ) =
	{
	val res = BigDecimal( s, bdmath.mc )
	
		if (res.isExactDouble)//isValidDouble)
			res.toDouble
		else
			res
	}
	
	def toBigDecimal( a: Number ): BigDecimal =
		a match
		{
			case bd: BigDecimal => bd
			case i: boxed.Integer => bigDecimal( i )
			case d: boxed.Double => bigDecimal( d )
			case r: Rational => bigDecimal( r )
			case bi: BigInt => bigDecimal( bi )
			case _ => sys.error( "can't convert from " + a )
		}
	
	def toComplexDouble( a: Number ): ComplexDouble =
		a match
		{
			case cd: ComplexDouble => cd
			case i: boxed.Integer => ComplexDouble( i.doubleValue )
			case d: boxed.Double => ComplexDouble( d )
			case r: Rational => ComplexDouble( r.doubleValue )
			case bi: BigInt => ComplexDouble( bi.doubleValue )
			case cbi: ComplexBigInt => ComplexDouble( cbi.re.doubleValue, cbi.im.doubleValue )
			case _ => sys.error( "can't convert from " + a )
		}
	
	def toComplexBigDecimal( a: Number ): ComplexBigDecimal =
		a match
		{
			case cd: ComplexBigDecimal => cd
			case bd: BigDecimal => new ComplexBigDecimal( bd, 0 )
			case i: boxed.Integer => new ComplexBigDecimal( i.asInstanceOf[Int], 0 )
			case d: boxed.Double => new ComplexBigDecimal( d.asInstanceOf[Double], 0 )
			case Rational( n, d ) =>
				val quo = toBigDecimal( n )/toBigDecimal( d )
				
				new ComplexBigDecimal( if (quo.precision > bdmath.mc.getPrecision) quo.round( bdmath.mc ) else quo, 0 )
			case bi: BigInt =>
				val len = digits( bi )
				val m = if (len >= bdmath.mc.getPrecision) mathContext(len + 5) else bdmath.mc

				new ComplexBigDecimal( BigDecimal(bi, m), 0 )
			case _ => sys.error( "can't convert from " + a )
		}
	
	def toComplexBigInt( a: Number ): ComplexBigInt =
		a match
		{
			case bi: BigInt => ComplexBigInt( bi )
			case i: boxed.Integer => ComplexBigInt( toBigInt(i) )
			case cbi: ComplexBigInt => cbi
			case _ => sys.error( "can't convert from " + a )
		}
	
	def toComplexRational( a: Number ): ComplexRational =
		a match
		{
			case bi: BigInt => ComplexRational( toRational(bi) )
			case i: boxed.Integer => ComplexRational( toRational(i) )
			case r: Rational => ComplexRational( r )
			case cbi: ComplexBigInt => ComplexRational( toRational(cbi.re), toRational(cbi.im) )
			case cr: ComplexRational => cr
			case _ => sys.error( "can't convert from " + a )
		}
	
	def toBigInt( a: Number ): BigInt =
		a match
		{
			case bi: BigInt => bi
			case i: boxed.Integer => BigInt( i )
			case _ => sys.error( "can't convert from " + a )
		}
	
	def toRational( a: Number ) =
		a match
		{
			case r: Rational => r
			case bi: BigInt => Rational( bi )
			case i: boxed.Integer => Rational( i )
			case _ => sys.error( "can't convert from " + a )
		}

	def maybeDemote( n: BigInt ): Number =
	  if (n.isValidInt)
	    n.toInt.asInstanceOf[Number]
	  else
	    n

	def maybeDemote( n: ComplexBigInt ): Number =
		if (n.im == 0)
			maybeDemote( n.re )
		else
			n

	def maybeDemote( n: ComplexRational ): Number =
		if (n.im.isZero)
			n.re.maybeDemote
		else
			n

	def maybePromote( n: Long ) =
		if (n.isValidInt)
			n.toInt.asInstanceOf[Number]
		else
			BigInt( n )

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
	
// 	def precision( p: Int )
// 	{
// 		require( p > 0, "precision is positive" )
// 		mc = mathContext( p )
// 	}
	
	def lookup( operation: Symbol ) = operations( operation )
	
	def apply( operation: Symbol, operands: Any* ) =
	{
		operations( operation )( operands map (_.getClass) )( operands.asInstanceOf[Seq[AnyRef]] )
	}
	
	def apply( operation: FunctionMap, operands: Any* ) =
	{
		operation( operands map (_.getClass) )( operands.asInstanceOf[Seq[AnyRef]] )
	}
	
	def predicate( operation: Symbol, operands: Any* ) = apply( operation, operands: _* ).asInstanceOf[Boolean]
	
	def predicate( operation: FunctionMap, operands: Any* ) = apply( operation, operands: _* ).asInstanceOf[Boolean]
}