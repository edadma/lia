package funl.lia


object Complex
{
	def apply( a: Int ) = new ComplexDouble( a, 0 )
	
	def apply( a: Long ) = new ComplexDouble( a, 0 )
	
	def apply( a: Double ) = new ComplexDouble( a, 0 )
	
	def apply( a: Double, b: Double ) = new ComplexDouble( a, b )
	
	def unapply( z: Any ): Option[(Double, Double)] =
		z match
		{
			case d: ComplexDouble => Some( d.a, d.b )
			case _ => None
		}
		
	implicit def double2complex( a: Double ) = Complex( a )
	
	implicit def double2imaginary( im: Double ) = new DoubleImaginary( im )
	
	class DoubleImaginary( val im: Double ) extends AnyVal
	{
		def i = new ComplexDouble( 0, im )
	}
}