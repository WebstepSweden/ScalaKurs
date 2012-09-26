package funsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.8/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  def over(elem: Int): Set = x => (x >= elem)
  def under(elem: Int): Set = x => (x <= elem)
  def range(start: Int, end: Int): Set = x => (start <= x) && (x <= end)

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2))
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains all elements in both set 1 and 2") {
    val s = intersect(under(7), over(3))
    assert(!contains(s, 2), "under range")
    assert(contains(s, 3), "in range lower end")
    assert(contains(s, 7), "in range upper end")
    assert(!contains(s, 8), "over range")
  }

  test("the set of all elements of `s` that are not in `t`") {
    val s = diff(range(3, 5), range(4, 7))
    assert(!contains(s, 2), "outside")
    assert(contains(s, 3), "in s and not in t")
    assert(!contains(s, 4), "in s but also t")
    assert(!contains(s, 7), "only in t")
  }

  test("Returns the subset of `s` for which `p` holds") {
    val s = filter(over(3), singletonSet(5))
    assert(!contains(s, 2), "under range")
    assert(!contains(s, 3), "in range, not in predicate")
    assert(contains(s, 5), "predicate")
  }

  test("Returns whether all bounded integers within `s` satisfy `p`") {
    assert(forall(range(3, 5), range(1, 9)))
  }

  test("Returns whether all bounded integers within `s` satisfy `p` 2") {
    assert(forall(range(3, 5), range(3, 5)))
  }

  test("Returns whether all bounded integers within `s` satisfy `p` 3") {
    assert(!forall(range(3, 5), range(4, 5)))
  }

  test("Returns whether all bounded integers within `s` satisfy `p` 4") {
    assert(!forall(range(3, 5), range(4, 8)))
  }

  test("Returns whether all bounded integers within `s` satisfy `p` 5") {
    assert(!forall(range(1, 9), range(3, 5)))
  }
  
  test("Exists") {
    assert(exists(range(3, 5), range(2, 4)))
  }

  test("Exists 2") {
    assert(exists(range(3, 5), range(4, 7)))
  }
  
  test("not Exists") {
    assert(!exists(range(3, 5), range(7, 8)))
  }
  
  test("map") {
    val s = map(range(3, 5), x => (x - 1))
    assert(!contains(s, 1))
    assert(contains(s, 2))
    assert(contains(s, 4))
    assert(!contains(s, 5))
  }

  test("map2") {
    val s = map(range(3, 5), x => (x * 2))
    assert(!contains(s, 5))
    assert(contains(s, 6))
    assert(contains(s, 10))
    assert(!contains(s, 11))
  }

  test("map3") {
    val s = map(singletonSet(1000), x => (x - 1))
    assert(contains(s, 999))
    assert(!contains(s, 1000))
  }
}
