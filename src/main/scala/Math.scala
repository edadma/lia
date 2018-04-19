package xyz.hyperreal.lia

import java.{lang => boxed}
import math._

import xyz.hyperreal.numbers._
import BigDecimalMath.decimal128._

object Math extends LIA
{
// 	special( ("BigInt", "xyz.hyperreal.numbers.ComplexInt") -> "xyz.hyperreal.numbers.ComplexBigInt" )
// 	special( ("Double", "xyz.hyperreal.numbers.ComplexInt") -> "xyz.hyperreal.numbers.ComplexDouble" )
// 	special( ("BigDecimal", "xyz.hyperreal.numbers.ComplexInt") -> "xyz.hyperreal.numbers.ComplexBigDecimal" )
// 	special( ("xyz.hyperreal.numbers.Rational", "xyz.hyperreal.numbers.ComplexInt") -> "xyz.hyperreal.numbers.ComplexRational" )
	special( ("Double", "xyz.hyperreal.numbers.ComplexBigInt") -> "xyz.hyperreal.numbers.ComplexDouble" )
	special( ("BigDecimal", "xyz.hyperreal.numbers.ComplexBigInt") -> "xyz.hyperreal.numbers.ComplexBigDecimal" )
	special( ("xyz.hyperreal.numbers.Rational", "xyz.hyperreal.numbers.ComplexBigInt") -> "xyz.hyperreal.numbers.ComplexRational" )
	special( ("Double", "xyz.hyperreal.numbers.ComplexRational") -> "xyz.hyperreal.numbers.ComplexDouble" )
	special( ("BigDecimal", "xyz.hyperreal.numbers.ComplexRational") -> "xyz.hyperreal.numbers.ComplexBigDecimal" )
	operation( '+,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => maybePromote( a.longValue + b.longValue )),
			"BigInt" -> (((a: Number), (b: Number)) => maybeDemote( toBigInt(a) + toBigInt(b) )),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) => (toRational(a) + toRational(b)).maybeDemote),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue + b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) + toBigDecimal(b)),
//			"xyz.hyperreal.numbers.ComplexInt" -> (((a: Number), (b: Number)) => maybeDemote( toComplexBigInt(a) + toComplexBigInt(b) )),
			"xyz.hyperreal.numbers.ComplexBigInt" -> (((a: Number), (b: Number)) => maybeDemote( toComplexBigInt(a) + toComplexBigInt(b) )),
			"xyz.hyperreal.numbers.ComplexRational" -> (((a: Number), (b: Number)) => maybeDemote( toComplexRational(a) + toComplexRational(b) )),
			"xyz.hyperreal.numbers.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) + toComplexDouble(b)),
			"xyz.hyperreal.numbers.ComplexBigDecimal" -> (((a: Number), (b: Number)) => toComplexBigDecimal(a) + toComplexBigDecimal(b)) ) )
	operation( '-,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => maybePromote( a.longValue - b.longValue )),
			"BigInt" -> (((a: Number), (b: Number)) => maybeDemote( toBigInt(a) - toBigInt(b) )),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) => (toRational(a) - toRational(b)).maybeDemote),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue - b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) - toBigDecimal(b)),
			"xyz.hyperreal.numbers.ComplexBigInt" -> (((a: Number), (b: Number)) => maybeDemote( toComplexBigInt(a) - toComplexBigInt(b) )),
			"xyz.hyperreal.numbers.ComplexRational" -> (((a: Number), (b: Number)) => maybeDemote( toComplexRational(a) - toComplexRational(b) )),
			"xyz.hyperreal.numbers.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) - toComplexDouble(b)),
			"xyz.hyperreal.numbers.ComplexBigDecimal" -> (((a: Number), (b: Number)) => toComplexBigDecimal(a) - toComplexBigDecimal(b)) ) ++
		unary(
			"Integer" -> ((a: boxed.Integer) => maybePromote( -a.longValue )),
			"BigInt" -> ((a: BigInt) => -a),
			"xyz.hyperreal.numbers.Rational" -> ((a: Rational) => -a),
			"Double" -> ((a: boxed.Double) => -a.doubleValue),
			"xyz.hyperreal.numbers.ComplexBigInt" -> ((a: ComplexBigInt) => -a),
			"xyz.hyperreal.numbers.ComplexRational" -> ((a: ComplexRational) => -a),
			"xyz.hyperreal.numbers.ComplexDouble" -> ((a: ComplexDouble) => -a),
			"xyz.hyperreal.numbers.ComplexBigDecimal" -> ((a: ComplexBigDecimal) => -a) ) )
	operation( '*,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => maybePromote( a.longValue * b.longValue )),
			"BigInt" -> (((a: Number), (b: Number)) => maybeDemote( toBigInt(a) * toBigInt(b) )),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) => (toRational(a) * toRational(b)).maybeDemote),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue * b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) * toBigDecimal(b)),
			"xyz.hyperreal.numbers.ComplexBigInt" -> (((a: Number), (b: Number)) => maybeDemote( toComplexBigInt(a) * toComplexBigInt(b) )),
			"xyz.hyperreal.numbers.ComplexRational" -> (((a: Number), (b: Number)) => maybeDemote( toComplexRational(a) * toComplexRational(b) )),
			"xyz.hyperreal.numbers.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) * toComplexDouble(b)),
			"xyz.hyperreal.numbers.ComplexBigDecimal" -> (((a: Number), (b: Number)) => toComplexBigDecimal(a) * toComplexBigDecimal(b)) ) )
	operation( '/,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => (toRational(a) / toRational(b)).maybeDemote),
			"BigInt" -> (((a: Number), (b: Number)) => (toRational(a) / toRational(b)).maybeDemote),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) => (toRational(a) / toRational(b)).maybeDemote),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue / b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) / toBigDecimal(b)),
			"xyz.hyperreal.numbers.ComplexBigInt" -> (((a: Number), (b: Number)) => maybeDemote( toComplexRational(a) / toComplexRational(b) )),
			"xyz.hyperreal.numbers.ComplexRational" -> (((a: Number), (b: Number)) => maybeDemote( toComplexRational(a) / toComplexRational(b) )),
			"xyz.hyperreal.numbers.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) / toComplexDouble(b)),
			"xyz.hyperreal.numbers.ComplexBigDecimal" -> (((a: Number), (b: Number)) => toComplexBigDecimal(a) / toComplexBigDecimal(b)) ) )
	operation( '//,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => a.doubleValue / b.doubleValue),
			"BigInt" -> (((a: Number), (b: Number)) => a.doubleValue / b.doubleValue),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) => a.doubleValue / b.doubleValue),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue / b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) / toBigDecimal(b)),
			"xyz.hyperreal.numbers.ComplexBigInt" -> (((a: Number), (b: Number)) => toComplexDouble(a) / toComplexDouble(b)),
			"xyz.hyperreal.numbers.ComplexRational" -> (((a: Number), (b: Number)) => toComplexDouble(a) / toComplexDouble(b)),
			"xyz.hyperreal.numbers.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) / toComplexDouble(b)),
			"xyz.hyperreal.numbers.ComplexBigDecimal" -> (((a: Number), (b: Number)) => toComplexBigDecimal(a) / toComplexBigDecimal(b)) ) )
	operation( '%,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => maybePromote( a.longValue % b.longValue )),
			"BigInt" -> (((a: Number), (b: Number)) => maybeDemote( toBigInt(a) % toBigInt(b) )),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue % b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) % toBigDecimal(b)) ) )
	operation( 'mod,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => maybePromote(
				{
				val m = b.longValue
				
					require( m >= 1, "modulus not positive" )
					
				val rem = a.longValue%m
				
					if (rem < 0)
						rem + m
					else
						rem
				} )),
      "BigInt" -> (((a: Number), (b: Number)) => maybeDemote( toBigInt(a) mod toBigInt(b) )) ) )
	operation( Symbol("\\"),
		binary(
			"Integer" -> (((a: Number), (b: Number)) => maybePromote( a.longValue / b.longValue )),
			"BigInt" -> (((a: Number), (b: Number)) => maybeDemote( toBigInt(a) / toBigInt(b) )) ) )
	operation( '|,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => b.longValue%a.longValue == 0),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(b)%toBigInt(a) == 0) ) )
	operation( '/|,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => b.longValue%a.longValue != 0),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(b)%toBigInt(a) != 0) ) )
	operation( 'not,
		unary(
			"Integer" -> ((a: Number) => ~a.intValue),
			"BigInt" -> ((a: Number) => ~toBigInt(a)) ) )
	operation( 'or,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => b.intValue|a.intValue),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a)|toBigInt(b)) ) )
	operation( 'xor,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => b.intValue^a.intValue),
			"BigInt" -> (((a: Number), (b: Number)) => maybeDemote( toBigInt(a)^toBigInt(b) )) ) )
	operation( 'and,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => b.intValue&a.intValue),
			"BigInt" -> (((a: Number), (b: Number)) => maybeDemote( toBigInt(a)&toBigInt(b) )) ) )
	operation( Symbol("\\%"),
		binary(
			"Integer" -> (((a: Number), (b: Number)) => (maybePromote( a.longValue / b.longValue ), maybePromote( a.longValue % b.longValue ))),
			"BigInt" -> (((a: Number), (b: Number)) => (maybeDemote( toBigInt(a) / toBigInt(b) ), maybeDemote( toBigInt(a) % toBigInt(b) ))) ) )
	operation( '^,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) =>
				{
				val res = BigInt( a ).pow( abs(b) )

					if (b < 0)
						Rational.oneOver( res ).maybeDemote
					else
						maybeDemote( res )
				} ),
			"BigInt" -> (((a: Number), (b: Number)) =>
				{
				val _a = toBigInt( a )
				val (bneg, res) =
					if (b.isInstanceOf[Int])
						(b.asInstanceOf[Int] < 0, _a.pow( abs(b.asInstanceOf[Int]) ))
					else
						(b.asInstanceOf[BigInt] < 0, Util.pow( _a, b.asInstanceOf[BigInt].abs ))
						
					if (bneg)
						Rational.oneOver( res ).maybeDemote
					else
						maybeDemote( res )
				} ),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) =>
				b match {
					case bi: boxed.Integer => (a.asInstanceOf[Rational] ^ bi).maybeDemote
					case bbi: BigInt => (a.asInstanceOf[Rational] ^ bbi).maybeDemote
					case br: Rational => pow( a.doubleValue, br.doubleValue )
				} ),
			"Double" -> (((a: Number), (b: Number)) =>
				{
				val _a = a.doubleValue
				
					if (_a < 0)
						toComplexDouble(a) ^ toComplexDouble(b)
					else
						math.pow( _a, b.doubleValue )
				} ),
			"BigDecimal" -> (((a: Number), (b: Number)) => BigDecimalMath.pow( toBigDecimal(a), toBigDecimal(b) )),
			"xyz.hyperreal.numbers.ComplexBigInt" -> (((a: Number), (b: Number)) =>
				b match {
					case p: boxed.Integer => a.asInstanceOf[ComplexBigInt] ^ p.intValue
					case (_: boxed.Double | _: Rational) => a.asInstanceOf[ComplexBigInt] ^ b.doubleValue
					case p: BigInt => a.asInstanceOf[ComplexBigInt] ^ p
					case p: BigDecimal => toComplexBigDecimal(a) ^ p
					case (_: ComplexRational | _: ComplexDouble | _: ComplexBigInt) => toComplexDouble(a) ^ toComplexDouble(b)
					case p: ComplexBigDecimal => toComplexBigDecimal(a) ^ p
				} ),
			"xyz.hyperreal.numbers.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) ^ toComplexDouble(b)),
			"xyz.hyperreal.numbers.ComplexBigDecimal" -> (((a: Number), (b: Number)) => toComplexBigDecimal(a) ^ toComplexBigDecimal(b)) ) )
	operation( '>,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a > b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) > toBigInt(b)),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) => toRational(a) > toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue > b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) > toBigDecimal(b)) ) )
	operation( '<,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a < b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) < toBigInt(b)),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) => toRational(a) < toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue < b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) < toBigDecimal(b)) ) )
	operation( '>=,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a >= b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) >= toBigInt(b)),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) => toRational(a) >= toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue >= b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) >= toBigDecimal(b)) ) )
	operation( '<=,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a <= b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) <= toBigInt(b)),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) => toRational(a) <= toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue <= b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) <= toBigDecimal(b)) ) )
	operation( '==,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a == b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) == toBigInt(b)),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) => toRational(a) == toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue == b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) == toBigDecimal(b)),
			"xyz.hyperreal.numbers.ComplexBigInt" -> (((a: Number), (b: Number)) => toComplexBigInt(a) == toComplexBigInt(b)),
			"xyz.hyperreal.numbers.ComplexRational" -> (((a: Number), (b: Number)) => toComplexRational(a) == toComplexRational(b)),
			"xyz.hyperreal.numbers.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) == toComplexDouble(b)),
			"xyz.hyperreal.numbers.ComplexBigDecimal" -> (((a: Number), (b: Number)) => toComplexBigDecimal(a) == toComplexBigDecimal(b)) ) )
	operation( '!=,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a != b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) != toBigInt(b)),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) => toRational(a) != toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue != b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) != toBigDecimal(b)),
			"xyz.hyperreal.numbers.ComplexBigInt" -> (((a: Number), (b: Number)) => toComplexBigInt(a) != toComplexBigInt(b)),
			"xyz.hyperreal.numbers.ComplexRational" -> (((a: Number), (b: Number)) => toComplexRational(a) != toComplexRational(b)),
			"xyz.hyperreal.numbers.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) != toComplexDouble(b)),
			"xyz.hyperreal.numbers.ComplexBigDecimal" -> (((a: Number), (b: Number)) => toComplexBigDecimal(a) != toComplexBigDecimal(b)) ) )
	operation( 'compare,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a compareTo b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) compareTo toBigInt(b)),
			"xyz.hyperreal.numbers.Rational" -> (((a: Number), (b: Number)) => toRational(a) compareTo toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue compareTo b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) compareTo toBigDecimal(b)) ) )
//			"xyz.hyperreal.numbers.ComplexBigInt" -> (((a: Number), (b: Number)) => toComplexBigInt(a) == toComplexBigInt(b)),
//			"xyz.hyperreal.numbers.ComplexRational" -> (((a: Number), (b: Number)) => toComplexRational(a) == toComplexRational(b)),
//			"xyz.hyperreal.numbers.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) == toComplexDouble(b)),
//			"xyz.hyperreal.numbers.ComplexBigDecimal" -> (((a: Number), (b: Number)) => toComplexBigDecimal(a) == toComplexBigDecimal(b)) ) )

	def sqrtFunction( n: Any ): Number =
		n match
		{
			case a: boxed.Integer =>
				{
				val n = abs( a )
				val dr = sqrt( n.toDouble )
				val ir = round(dr).toInt

					if (ir*ir == n)
						if (a < 0) new ComplexBigInt( 0, ir ) else new boxed.Integer( ir )
					else
						if (a < 0) new ComplexDouble( 0, dr ) else new boxed.Double( dr )
				}
			case a: BigInt =>
				{
					bisqrt( a.abs ) match
					{
						case Left( ir ) => if (a < 0) new ComplexBigInt( 0, ir ) else maybeDemote( ir )
						case Right( dr ) => if (a < 0) new ComplexDouble( 0, dr.doubleValue ) else dr
					}
				}
			case a: Rational =>
				{
				val ar = a.abs

					def rsqrt = if (a < 0) new ComplexDouble( 0, sqrt(ar.doubleValue) ) else new boxed.Double( sqrt(ar.doubleValue) )

					bisqrt( ar.n ) match
					{
						case Left( irn ) =>
							bisqrt( ar.d ) match
							{
								case Left( ird ) =>
									val res = Rational( irn, ird )

									if (a < 0) new ComplexRational( 0, res ) else res
								case _ => rsqrt
							}
						case _ => rsqrt
					}
				}
			case a: boxed.Double => if (a < 0) new ComplexDouble( 0, sqrt(-a) ) else new boxed.Double( sqrt(a) )
			case a: BigDecimal => if (a < 0) new ComplexBigDecimal( 0, BigDecimalMath.sqrt(-a) ) else BigDecimalMath.sqrt( a )
			case a: ComplexBigInt => a.sqrt
			case a: ComplexRational => a.sqrt
			case a: ComplexDouble => a.sqrt
			case a: ComplexBigDecimal => a.sqrt
		}

	def absFunction( n: Any ): Number =
		n match {
			case a: boxed.Integer => maybePromote( abs(a.longValue) )
			case a: BigInt => maybeDemote( a.abs )
			case a: xyz.hyperreal.numbers.Rational => a.abs
			case a: boxed.Double => new boxed.Double( abs(a) )
			case a: BigDecimal => a.abs
			case a: ComplexBigInt => a.abs
			case a: ComplexRational => a.abs
			case a: ComplexDouble => new boxed.Double( a.abs )
			case a: ComplexBigDecimal => a.abs
		}

	def floorFunction( n: Any ): Number =
		n match {
			case a: boxed.Integer => a
			case a: BigInt => maybeDemote( a )
			case a: xyz.hyperreal.numbers.Rational => a.floor
			case a: boxed.Double =>
				val f = BigDecimal( a ).setScale( 0, BigDecimal.RoundingMode.FLOOR )

				if (f.isValidInt)
					f.toIntExact
				else
					f.toBigInt
			case a: BigDecimal => a.setScale( 0, BigDecimal.RoundingMode.FLOOR )
			case a: ComplexBigInt => a
			case a: ComplexRational => a.floor
			case a: ComplexDouble => a.floor
			case a: ComplexBigDecimal => a.floor
		}

	def ceilFunction( n: Any ): Number =
		n match {
			case a: boxed.Integer => a
			case a: BigInt => maybeDemote( a )
			case a: xyz.hyperreal.numbers.Rational => a.ceil
			case a: boxed.Double =>
				val f = BigDecimal( a ).setScale( 0, BigDecimal.RoundingMode.CEILING )

				if (f.isValidInt)
					f.toIntExact
				else
					f.toBigInt
			case a: BigDecimal => a.setScale( 0, BigDecimal.RoundingMode.CEILING )
			case a: ComplexBigInt => a
			case a: ComplexRational => a.ceil
			case a: ComplexDouble => a.ceil
			case a: ComplexBigDecimal => a.ceil
		}

	def cosFunction( n: Any ): Number =
		n match {
			case (_: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double) => cos( n.asInstanceOf[Number].doubleValue ).asInstanceOf[boxed.Double]
			case a: BigDecimal => BigDecimalMath.cos( a )
			case a: ComplexBigInt => a.cos
			case a: ComplexRational => a.cos
			case a: ComplexDouble => a.cos
			case a: ComplexBigDecimal => a.cos
		}

	def sinFunction( n: Any ): Number =
		n match {
			case (_: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double) => sin( n.asInstanceOf[Number].doubleValue ).asInstanceOf[boxed.Double]
			case a: BigDecimal => BigDecimalMath.sin( a )
			case a: ComplexBigInt => a.sin
			case a: ComplexRational => a.sin
			case a: ComplexDouble => a.sin
			case a: ComplexBigDecimal => a.sin
		}

	def acosFunction( n: Any ): Number =
		n match {
			case (_: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double) => acos( n.asInstanceOf[Number].doubleValue ).asInstanceOf[boxed.Double]
			case a: BigDecimal => BigDecimalMath.acos( a )
			case a: ComplexBigInt => a.acos
			case a: ComplexRational => a.acos
			case a: ComplexDouble => a.acos
			case a: ComplexBigDecimal => a.acos
		}

	def asinFunction( n: Any ): Number =
		n match {
			case (_: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double) => asin( n.asInstanceOf[Number].doubleValue ).asInstanceOf[boxed.Double]
			case a: BigDecimal => BigDecimalMath.asin( a )
			case a: ComplexBigInt => a.asin
			case a: ComplexRational => a.asin
			case a: ComplexDouble => a.asin
			case a: ComplexBigDecimal => a.asin
		}

  def expFunction( n: Any ): Number =
    n match {
      case (_: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double) => exp( n.asInstanceOf[Number].doubleValue ).asInstanceOf[boxed.Double]
      case a: BigDecimal => BigDecimalMath.exp( a )
			case a: ComplexBigInt => a.exp
			case a: ComplexRational => a.exp
      case a: ComplexDouble => a.exp
      case a: ComplexBigDecimal => a.exp
    }
}
