package ca.hyperreal.lia

import java.{lang => boxed}
import math.{sqrt => sqr, round => rnd, abs => ab, exp => ex, pow => po, _}


object Math extends LIA
{
// 	special( ("BigInt", "Double") -> "BigDecimal" )
// 	special( ("Double", "BigInt") -> "BigDecimal" )
// 	special( ("ca.hyperreal.lia.Rational", "Double") -> "BigDecimal" )
// 	special( ("Double", "ca.hyperreal.lia.Rational") -> "BigDecimal" )
// 	special( ("BigInt", "ComplexDouble") -> "ca.hyperreal.lia.ComplexDecimal" )
// 	special( ("ComplexDouble", "BigInt") -> "ca.hyperreal.lia.ComplexDecimal" )
// 	special( ("ca.hyperreal.lia.Rational", "ca.hyperreal.lia.ComplexDouble") -> "ca.hyperreal.lia.ComplexDecimal" )
// 	special( ("ca.hyperreal.lia.ComplexDouble", "ca.hyperreal.lia.Rational") -> "ca.hyperreal.lia.ComplexDecimal" )
	operation( '+,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => maybePromote( a.longValue + b.longValue )),
			"BigInt" -> (((a: Number), (b: Number)) => maybeDemote( toBigInt(a) + toBigInt(b) )),
			"ca.hyperreal.lia.Rational" -> (((a: Number), (b: Number)) => (toRational(a) + toRational(b)).maybeDemote),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue + b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) + toBigDecimal(b)),
			"ca.hyperreal.lia.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) + toComplexDouble(b)),
			"ca.hyperreal.lia.ComplexDecimal" -> (((a: Number), (b: Number)) => toComplexDecimal(a) + toComplexDecimal(b)) ) )
	operation( '-,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => maybePromote( a.longValue - b.longValue )),
			"BigInt" -> (((a: Number), (b: Number)) => maybeDemote( toBigInt(a) - toBigInt(b) )),
			"ca.hyperreal.lia.Rational" -> (((a: Number), (b: Number)) => (toRational(a) - toRational(b)).maybeDemote),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue - b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) - toBigDecimal(b)),
			"ca.hyperreal.lia.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) - toComplexDouble(b)),
			"ca.hyperreal.lia.ComplexDecimal" -> (((a: Number), (b: Number)) => toComplexDecimal(a) - toComplexDecimal(b)) ) ++
		unary(
			"Integer" -> ((a: boxed.Integer) => maybePromote( -a.longValue )),
			"BigInt" -> ((a: BigInt) => -a),
			"ca.hyperreal.lia.Rational" -> ((a: Rational) => -a),
			"Double" -> ((a: boxed.Double) => -a.doubleValue),
			"ca.hyperreal.lia.ComplexDouble" -> ((a: ComplexDouble) => -a),
			"ca.hyperreal.lia.ComplexDecimal" -> ((a: ComplexDecimal) => -a) ) )
	operation( '*,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => maybePromote( a.longValue * b.longValue )),
			"BigInt" -> (((a: Number), (b: Number)) => maybeDemote( toBigInt(a) * toBigInt(b) )),
			"ca.hyperreal.lia.Rational" -> (((a: Number), (b: Number)) => (toRational(a) * toRational(b)).maybeDemote),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue * b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) * toBigDecimal(b)),
			"ca.hyperreal.lia.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) * toComplexDouble(b)),
			"ca.hyperreal.lia.ComplexDecimal" -> (((a: Number), (b: Number)) => toComplexDecimal(a) * toComplexDecimal(b)) ) )
	operation( '/,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => (toRational(a) / toRational(b)).maybeDemote),
			"BigInt" -> (((a: Number), (b: Number)) => (toRational(a) / toRational(b)).maybeDemote),
			"ca.hyperreal.lia.Rational" -> (((a: Number), (b: Number)) => (toRational(a) / toRational(b)).maybeDemote),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue / b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) / toBigDecimal(b)),
			"ca.hyperreal.lia.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) / toComplexDouble(b)),
			"ca.hyperreal.lia.ComplexDecimal" -> (((a: Number), (b: Number)) => toComplexDecimal(a) / toComplexDecimal(b)) ) )
	operation( '//,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => a.doubleValue / b.doubleValue),
			"BigInt" -> (((a: Number), (b: Number)) => a.doubleValue / b.doubleValue),
			"ca.hyperreal.lia.Rational" -> (((a: Number), (b: Number)) => a.doubleValue / b.doubleValue),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue / b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) / toBigDecimal(b)),
			"ca.hyperreal.lia.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) / toComplexDouble(b)),
			"ca.hyperreal.lia.ComplexDecimal" -> (((a: Number), (b: Number)) => toComplexDecimal(a) / toComplexDecimal(b)) ) )
	operation( '%,
		binary(
			"Integer" -> (((a: Number), (b: Number)) => maybePromote( a.longValue % b.longValue )),
			"BigInt" -> (((a: Number), (b: Number)) => maybeDemote( toBigInt(a) % toBigInt(b) )) ) )
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
				val res = BigInt( a ).pow( ab(b) )

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
						(b.asInstanceOf[Int] < 0, _a.pow( ab(b.asInstanceOf[Int]) ))
					else
						(b.asInstanceOf[BigInt] < 0, Util.pow( _a, b.asInstanceOf[BigInt].abs ))
						
					if (bneg)
						Rational.oneOver( res ).maybeDemote
					else
						maybeDemote( res )
				} ),
			"ca.hyperreal.lia.Rational" -> (((a: Number), (b: Number)) =>
				b match
				{
					case bi: boxed.Integer => (a.asInstanceOf[Rational] ^ bi).maybeDemote
					case bbi: BigInt => (a.asInstanceOf[Rational] ^ bbi).maybeDemote
					case br: Rational => po( a.doubleValue, br.doubleValue )
				} ),
			"Double" -> (((a: Number), (b: Number)) =>
				{
				val _a = a.doubleValue
				
					if (_a < 0)
						toComplexDouble(a) ^ toComplexDouble(b)
					else
						math.pow( _a, b.doubleValue )
				} ),
			"BigDecimal" -> (((a: Number), (b: Number)) => pow( toBigDecimal(a), toBigDecimal(b) )),
			"ca.hyperreal.lia.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) ^ toComplexDouble(b)),
			"ca.hyperreal.lia.ComplexDecimal" -> (((a: Number), (b: Number)) => toComplexDecimal(a) ^ toComplexDecimal(b)) ) )
	operation( '>,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a > b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) > toBigInt(b)),
			"ca.hyperreal.lia.Rational" -> (((a: Number), (b: Number)) => toRational(a) > toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue > b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) > toBigDecimal(b)) ) )
	operation( '<,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a < b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) < toBigInt(b)),
			"ca.hyperreal.lia.Rational" -> (((a: Number), (b: Number)) => toRational(a) < toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue < b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) < toBigDecimal(b)) ) )
	operation( '>=,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a >= b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) >= toBigInt(b)),
			"ca.hyperreal.lia.Rational" -> (((a: Number), (b: Number)) => toRational(a) >= toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue >= b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) >= toBigDecimal(b)) ) )
	operation( '<=,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a <= b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) <= toBigInt(b)),
			"ca.hyperreal.lia.Rational" -> (((a: Number), (b: Number)) => toRational(a) <= toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue <= b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) <= toBigDecimal(b)) ) )
	operation( '==,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a == b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) == toBigInt(b)),
			"ca.hyperreal.lia.Rational" -> (((a: Number), (b: Number)) => toRational(a) == toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue == b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) == toBigDecimal(b)),
			"ca.hyperreal.lia.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) == toComplexDouble(b)),
			"ca.hyperreal.lia.ComplexDecimal" -> (((a: Number), (b: Number)) => toComplexDecimal(a) == toComplexDecimal(b)) ) )
	operation( '!=,
		binary(
			"Integer" -> (((a: boxed.Integer), (b: boxed.Integer)) => a != b),
			"BigInt" -> (((a: Number), (b: Number)) => toBigInt(a) != toBigInt(b)),
			"ca.hyperreal.lia.Rational" -> (((a: Number), (b: Number)) => toRational(a) != toRational(b)),
			"Double" -> (((a: Number), (b: Number)) => a.doubleValue != b.doubleValue),
			"BigDecimal" -> (((a: Number), (b: Number)) => toBigDecimal(a) != toBigDecimal(b)),
			"ca.hyperreal.lia.ComplexDouble" -> (((a: Number), (b: Number)) => toComplexDouble(a) != toComplexDouble(b)),
			"ca.hyperreal.lia.ComplexDecimal" -> (((a: Number), (b: Number)) => toComplexDecimal(a) != toComplexDecimal(b)) ) )
	
	def sqrtFunction( n: Any ): Number =
		n match
		{
			case a: boxed.Integer =>
				{
				val n = ab( a )
				val dr = sqr( n.toDouble )
				val ir = rnd(dr).toInt

					if (ir*ir == n)
						if (a < 0) new ComplexDouble( 0, ir ) else new boxed.Integer( ir )
					else
						if (a < 0) new ComplexDouble( 0, dr ) else new boxed.Double( dr )
				}
			case a: BigInt =>
				{
					bisqrt( a.abs ) match
					{
						case Left( ir ) => if (a < 0) new ComplexDecimal( 0, bigDecimal(ir), this ) else maybeDemote( ir )
						case Right( dr ) => if (a < 0) new ComplexDecimal( 0, dr, this ) else dr
					}
				}
			case a: ca.hyperreal.lia.Rational =>
				{
				val ar = a.abs

					def rsqrt = if (a < 0) new ComplexDecimal( 0, sqrt( ar.decimalValue(Math) ), this ) else sqrt( ar.decimalValue(Math) )

					bisqrt( ar.n ) match
					{
						case Left( irn ) =>
							bisqrt( ar.d ) match
							{
								case Left( ird ) =>
									val res = Rational( irn, ird )

									if (a < 0) new ComplexDecimal( 0, bigDecimal(res), this ) else res
								case _ => rsqrt
							}
						case _ => rsqrt
					}
				}
			case a: boxed.Double => if (a < 0) new ComplexDouble( 0, sqr(-a) ) else new boxed.Double( sqr(a) )
			case a: BigDecimal => if (a < 0) new ComplexDecimal( 0, sqrt(-a), this ) else sqrt( a )
			case a: ComplexDouble => a.sqrt
			case a: ComplexDecimal => a.sqrt
		}

	def absFunction( n: Any ): Number =
		n match
		{
			case a: boxed.Integer => maybePromote( ab(a.longValue) )
			case a: BigInt => maybeDemote( a.abs )
			case a: ca.hyperreal.lia.Rational => a.abs
			case a: boxed.Double => new boxed.Double( ab(a) )
			case a: BigDecimal => a.abs
			case a: ComplexDouble => new boxed.Double( a.abs )
			case a: ComplexDecimal => a.abs
		}

	def cosFunction( n: Any ): Number =
		n match
		{
			case a: boxed.Integer => cos( a.doubleValue ).asInstanceOf[boxed.Double]
			case a: BigInt => cosBigDecimal( bigDecimal(a) )
			case a: ca.hyperreal.lia.Rational => cosBigDecimal( bigDecimal(a) )
			case a: boxed.Double => new boxed.Double( cos(a) )
			case a: BigDecimal => cosBigDecimal( a )
			case a: ComplexDouble => a.cos
			case a: ComplexDecimal => a.cos
		}

	def sinFunction( n: Any ): Number =
		n match
		{
			case a: boxed.Integer => sin( a.doubleValue ).asInstanceOf[boxed.Double]
			case a: BigInt => sinBigDecimal( bigDecimal(a) )
			case a: ca.hyperreal.lia.Rational => sinBigDecimal( bigDecimal(a) )
			case a: boxed.Double => new boxed.Double( sin(a) )
			case a: BigDecimal => sinBigDecimal( a )
			case a: ComplexDouble => a.sin
			case a: ComplexDecimal => a.sin
		}

	def acosFunction( n: Any ): Number =
		n match
		{
			case a: boxed.Integer => acos( a.doubleValue ).asInstanceOf[boxed.Double]
			case a: BigInt => acosBigDecimal( bigDecimal(a) )
			case a: ca.hyperreal.lia.Rational => acosBigDecimal( bigDecimal(a) )
			case a: boxed.Double => new boxed.Double( acos(a) )
			case a: BigDecimal => acosBigDecimal( a )
			case a: ComplexDouble => a.acos
			case a: ComplexDecimal => a.acos
		}

	def asinFunction( n: Any ): Number =
		n match
		{
			case a: boxed.Integer => asin( a.doubleValue ).asInstanceOf[boxed.Double]
//			case a: BigInt => asinBigDecimal( bigDecimal(a) )
//			case a: ca.hyperreal.lia.Rational => asinBigDecimal( bigDecimal(a) )
			case a: boxed.Double => new boxed.Double( asin(a) )
//			case a: BigDecimal => asinBigDecimal( a )
			case a: ComplexDouble => a.asin
			case a: ComplexDecimal => a.asin
		}

  def expFunction( n: Any ): Number =
    n match
    {
      case a: boxed.Integer => ex( a.doubleValue ).asInstanceOf[boxed.Double]
 //     case a: BigInt => expBigDecimal( bigDecimal(a) )
      case a: ca.hyperreal.lia.Rational => ex( a.doubleValue ).asInstanceOf[boxed.Double]
      case a: boxed.Double => new boxed.Double( ex(a) )
      case a: BigDecimal => expBigDecimal( a )
      case a: ComplexDouble => a.exp
      case a: ComplexDecimal => a.exp
    }
}
