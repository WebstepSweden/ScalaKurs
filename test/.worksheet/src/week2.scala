object week2 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(186); 

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  };System.out.println("""sum: (f: Int => Int)(a: Int, b: Int)Int""");$skip(173); 

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, pr: Int): Int = {
      if (a > b) pr
      else loop(a + 1, pr * f(a))
    }
    loop(a, 1)
  };System.out.println("""product: (f: Int => Int)(a: Int, b: Int)Int""");$skip(50); 


  def fact(n: Int): Int = product(x => x)(1, n);System.out.println("""fact: (n: Int)Int""");$skip(226); 


  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    def loop(a: Int, pr: Int): Int = {
      if (a > b) pr
      else loop(a + 1, combine(pr, f(a)))
    }
    loop(a, zero)
  };System.out.println("""mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int)Int""");$skip(88); 
  def sum2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b);System.out.println("""sum2: (f: Int => Int)(a: Int, b: Int)Int""");$skip(92); 
  def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b);System.out.println("""product2: (f: Int => Int)(a: Int, b: Int)Int""");$skip(71); 
  def fact2(n: Int): Int = mapReduce(x => x, (x, y) => x * y, 1)(1, n);System.out.println("""fact2: (n: Int)Int""");$skip(25); val res$0 = 

  sum(x => x * x)(0, 5);System.out.println("""res0: Int = """ + $show(res$0));$skip(50); val res$1 = 
  mapReduce(x => x * x, (x, y) => x + y, 0)(0, 5);System.out.println("""res1: Int = """ + $show(res$1));$skip(25); val res$2 = 
  sum2(x => x * x)(0, 5);System.out.println("""res2: Int = """ + $show(res$2));$skip(31); val res$3 = 
  
  product(x => x * x)(3, 4);System.out.println("""res3: Int = """ + $show(res$3));$skip(50); val res$4 = 
  mapReduce(x => x * x, (x, y) => x * y, 1)(3, 4);System.out.println("""res4: Int = """ + $show(res$4));$skip(29); val res$5 = 
  product2(x => x * x)(3, 4);System.out.println("""res5: Int = """ + $show(res$5));$skip(11); val res$6 = 

  fact(4);System.out.println("""res6: Int = """ + $show(res$6));$skip(46); val res$7 = 
  mapReduce(x => x, (x, y) => x * y, 1)(1, 4);System.out.println("""res7: Int = """ + $show(res$7));$skip(11); val res$8 = 
  fact2(4);System.out.println("""res8: Int = """ + $show(res$8))}
}