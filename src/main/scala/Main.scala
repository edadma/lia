package funl.lia

import math._


object Main extends App
{	
	def time( action: => Unit )
	{
	val start = compat.Platform.currentTime
	
		action
		println( "time: " + (compat.Platform.currentTime - start) )
	}
	
// 	val x = Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v*Math.E.v
// 	
// 	System.gc
// 	System.gc
// 	
// 	time( println( Math.ln(1) ) )
// 
// 	System.gc
// 	System.gc
// 
// 	time( println( Math.ln(1) ) )
// 	time( println( Math.ln(x) ) )
// 	time( println( Math.ln(1.1), log(1.1) ) )

	println( Math('not, 1) )
}

// 	def agm( x: Double, y: Double ) =
// 	{
// 		def am( a: Double, b: Double ) = (a + b)/2
//
// 		def gm( a: Double, b: Double ) = sqrt( a*b )
//
// 		def recur( an: Double, gn: Double ): Double =
// 		{
// 		val anp1 = am( an, gn )
// 		val gnp1 = gm( an, gn )
//
// //			if ((anp1 - gnp1).abs < 1e-16)
// 			if (anp1 == gnp1)
// 				anp1
// 			else
// 				recur( anp1, gnp1 )
// 		}
//
// 		recur( am(x, y), gm(x, y) )
// 	}
//
// 	// sum( k >= 1, 1/(k*(2^k)) )
// 	def ln2 =
// 	{
// 	var res = 0.0
// 	var p3 = 3L
// 	var p4 = 4L
// 	var term = 1.0/3 + 1.0/4
// 	var k = 1
//
// 		while (term >= 1e-16)
// 		{
// 			res += term
// 			p3 *= 3
// 			p4 *= 4
// 			k += 1
// 			term = (1.0/p3 + 1.0/p4)/k
// 			println( res, term )
// 		}
//
// 		res
// 	}

// object Main extends App
// {
// 	val m =
// 		new LIA
// 		{
// 			def algln( x: BigDecimal ) =
// 			{
// 				def a( n: Int, a0: BigDecimal, g0: BigDecimal ): BigDecimal =
// 					if (n == 0)
// 						a0
// 					else
// 						(a(n - 1, a0, g0) + g(n - 1, a0, g0))/2
// 
// 				def g( n: Int, a0: BigDecimal, g0: BigDecimal ): BigDecimal =
// 					if (n == 0)
// 						g0
// 					else
// 						sqrt(a(n, a0, g0)*g(n - 1, a0, g0))
// 						
// 				def d( k: Int, n: Int, a0: BigDecimal, g0: BigDecimal ): BigDecimal =
// 					if (k == 0)
// 						a( n, a0, g0 )
// 					else
// 					{
// 					val p = TWO.v.pow( -2*k )
// 					
// 						(d( k - 1, n, a0, g0 ) - p*d( k - 1, n - 1, a0, g0 ))/(1 - p)
// 					}
// 					
// 			val n = 9
// 				
// 				(x - 1)/d( n, n, (1 + x)/2, sqrt(x) )
// 			}
// 		}
// 	
// 	println( "----------" )
// 	println( m.algln(m.exp(10)) )
// 	println( "----------" )
// 	println( m.ln(m.exp(10)) )
// }