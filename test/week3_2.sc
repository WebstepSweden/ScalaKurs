object week3_2 {

	val a = new NonEmpty(5, new Empty, new Empty)
                                                  //> a  : NonEmpty = {.5.}
	val b = a.incl(3).incl(4)                 //> b  : IntSet = {{.3{.4.}}5.}

	val c = new NonEmpty(4, new Empty, new Empty)
                                                  //> c  : NonEmpty = {.4.}
                                                  
  b union c                                       //> res0: IntSet = {{.3.}4{.5.}}
  
  new Empty union new Empty                       //> res1: IntSet = .
  
  new NonEmpty(5, new Empty, new Empty) union new Empty()
                                                  //> res2: IntSet = {.5.}
                                                  
  if (true) 1 else false                          //> res3: AnyVal = 1
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