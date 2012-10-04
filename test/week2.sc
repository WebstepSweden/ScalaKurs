object week2 {

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }                                               //> sum: (f: Int => Int)(a: Int, b: Int)Int

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, pr: Int): Int = {
      if (a > b) pr
      else loop(a + 1, pr * f(a))
    }
    loop(a, 1)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int


  def fact(n: Int): Int = product(x => x)(1, n)   //> fact: (n: Int)Int


  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    def loop(a: Int, pr: Int): Int = {
      if (a > b) pr
      else loop(a + 1, combine(pr, f(a)))
    }
    loop(a, zero)
  }                                               //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int
  def sum2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b)
                                                  //> sum2: (f: Int => Int)(a: Int, b: Int)Int
  def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
                                                  //> product2: (f: Int => Int)(a: Int, b: Int)Int
  def fact2(n: Int): Int = mapReduce(x => x, (x, y) => x * y, 1)(1, n)
                                                  //> fact2: (n: Int)Int

  sum(x => x * x)(0, 5)                           //> res0: Int = 55
  mapReduce(x => x * x, (x, y) => x + y, 0)(0, 5) //> res1: Int = 55
  sum2(x => x * x)(0, 5)                          //> res2: Int = 55
  
  product(x => x * x)(3, 4)                       //> res3: Int = 144
  mapReduce(x => x * x, (x, y) => x * y, 1)(3, 4) //> res4: Int = 144
  product2(x => x * x)(3, 4)                      //> res5: Int = 144

  fact(4)                                         //> res6: Int = 24
  mapReduce(x => x, (x, y) => x * y, 1)(1, 4)     //> res7: Int = 24
  fact2(4)                                        //> res8: Int = 24
}