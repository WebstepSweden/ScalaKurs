object week3 {
  
	val a = new Rational(1, 2)                //> a  : Rational = 1/2
	a.neg                                     //> res0: Rational = 1/-2
	a.sub(new Rational(1, 4))                 //> res1: Rational = 1/4

	val x = new Rational(1, 3)                //> x  : Rational = 1/3
	val y = new Rational(5, 7)                //> y  : Rational = 5/7
	val z = new Rational(3, 2)                //> z  : Rational = 3/2
	
	x.sub(y).sub(z)                           //> res2: Rational = -79/42
	
	new Rational(1, 4).less(new Rational(1,3))//> res3: Boolean = true
	
	new Rational(1, 4).max(new Rational(1,3)) //> res4: Rational = 1/3
	
}

class Rational(x: Int, y: Int) {
  require (y != 0)
  
  def this(x : Int) = this(x, 1)
  
  private def gcd(a : Int, b : Int): Int = if (b == 0) a else gcd(b, a % b)
  private def g = gcd(x, y)
	def numer = x
	def denom = y
	
	def neg = new Rational(-numer, denom)
	def less(that : Rational) = numer * that.denom < that.numer * denom
	def max(that: Rational) = if (less(that)) that else this
	def add(that : Rational) = new Rational(numer * that.denom + that.numer * denom, that.denom * denom)
	def sub(that : Rational) = add(that.neg)
	
	override def toString = numer/g + "/" + denom/g
}