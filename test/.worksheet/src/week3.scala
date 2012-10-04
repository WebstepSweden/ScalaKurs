object week3 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(45); 
  
	val a = new Rational(1, 2);System.out.println("""a  : Rational = """ + $show(a ));$skip(7); val res$0 = 
	a.neg;System.out.println("""res0: Rational = """ + $show(res$0));$skip(27); val res$1 = 
	a.sub(new Rational(1, 4));System.out.println("""res1: Rational = """ + $show(res$1));$skip(29); 

	val x = new Rational(1, 3);System.out.println("""x  : Rational = """ + $show(x ));$skip(28); 
	val y = new Rational(5, 7);System.out.println("""y  : Rational = """ + $show(y ));$skip(28); 
	val z = new Rational(3, 2);System.out.println("""z  : Rational = """ + $show(z ));$skip(19); val res$2 = 
	
	x.sub(y).sub(z);System.out.println("""res2: Rational = """ + $show(res$2));$skip(46); val res$3 = 
	
	new Rational(1, 4).less(new Rational(1,3));System.out.println("""res3: Boolean = """ + $show(res$3));$skip(45); val res$4 = 
	
	new Rational(1, 4).max(new Rational(1,3));System.out.println("""res4: Rational = """ + $show(res$4))}
	
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