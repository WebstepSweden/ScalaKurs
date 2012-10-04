object week3_2 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(64); 

	val a = new NonEmpty(5, new Empty, new Empty);System.out.println("""a  : NonEmpty = """ + $show(a ));$skip(27); 
	val b = a.incl(3).incl(4);System.out.println("""b  : IntSet = """ + $show(b ));$skip(48); 

	val c = new NonEmpty(4, new Empty, new Empty);System.out.println("""c  : NonEmpty = """ + $show(c ));$skip(63); val res$0 = 
                                                  
  b union c;System.out.println("""res0: IntSet = """ + $show(res$0));$skip(31); val res$1 = 
  
  new Empty union new Empty;System.out.println("""res1: IntSet = """ + $show(res$1));$skip(61); val res$2 = 
  
  new NonEmpty(5, new Empty, new Empty) union new Empty();System.out.println("""res2: IntSet = """ + $show(res$2));$skip(76); val res$3 = 
                                                  
  if (true) 1 else false;System.out.println("""res3: AnyVal = """ + $show(res$3))}
}


abstract class IntSet {
	def incl(x: Int): IntSet
	def contains(x: Int): Boolean
	def union(other: IntSet): IntSet
}

class Empty extends IntSet {
	def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
	def contains(x: Int): Boolean = false
	def union(other: IntSet): IntSet = other
	override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
	def incl(x: Int): IntSet =
	  if (x < elem) new NonEmpty(elem, left incl x, right)
	  else if (x > elem) new NonEmpty(elem, left, right incl x)
	  else this
	  
	def contains(x: Int): Boolean =
	  if (x < elem) left contains x
	  else if (x > elem) right contains x
	  else true
	  
	def union(other: IntSet): IntSet =
	  ((left union right) union other) incl elem
	
	override def toString = "{" + left + elem + right + "}"
}