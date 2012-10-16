package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times test") {
    assert(times(List('a', 'a', 'b', 'b', 'c', 'a')) === List(('a', 3), ('b', 2), ('c', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until test 0") {
    intercept[IllegalArgumentException] {
      val leafList = List()
      until(singleton, combine)(leafList)
    }
  }

  test("until test 1") {
    val leafList = List(Leaf('a', 1))
    assert(until(singleton, combine)(leafList) === List((Leaf('a', 1))))
  }

  test("until test 2") {
    val leafList = List(Leaf('a', 1), Leaf('b', 2))
    assert(until(singleton, combine)(leafList) === List(Fork(Leaf('a', 1), Leaf('b', 2), List('a', 'b'), 3)))
  }

  test("until test 3") {
    val leafList = List(Leaf('a', 10), Leaf('b', 1), Leaf('c', 5))
    val expected = List(Fork(Leaf('c', 5), Fork(Leaf('a', 10), Leaf('b', 1), List('a', 'b'), 11), List('c', 'a', 'b'), 16))
    assert(until(singleton, combine)(leafList) === expected)
  }

  test("create code tree") {
    val chars = string2Chars("abrakadabra")
    val tree = createCodeTree(chars)
    val expected = Fork(Leaf('a', 5), Fork(Leaf('r', 2), Fork(Fork(Leaf('k', 1), Leaf('d', 1), List('k', 'd'), 2), Leaf('b', 2), List('k', 'd', 'b'), 4), List('r', 'k', 'd', 'b'), 6), List('a', 'r', 'k', 'd', 'b'), 11)
    assert(tree === expected)
  }

  test("encode test") {
    val chars = string2Chars("abrakadabra")
    val tree = createCodeTree(chars)
    val charList = string2Chars("abba")
    val encoded = encode(tree)(charList)
    assert(encoded === List(0, 1, 1, 1, 1, 1, 1, 0))
  }

  test("decode test") {
    val chars = string2Chars("ab")
    val tree = createCodeTree(chars)
    val bitList = List(0, 0)
    val decoded = decode(tree, bitList)
    assert(decoded === List('a', 'a'))
  }

  test("decoded french secret") {
    val secret = decodedSecret.mkString
    assert(secret === "huffmanestcool")
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a long text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("abbddbabdbdadabbdbadbdabadb".toList)) === "abbddbabdbdadabbdbadbdabadb".toList)
    }
  }

  test("convert test") {
    val chars = string2Chars("abrakadabra")
    val tree = createCodeTree(chars)
    val table = convert(tree)
    val expected = List(('a', List(0)), ('r', List(1, 0)), ('k', List(1, 1, 0, 0)), ('d', List(1, 1, 0, 1)), ('b', List(1, 1, 1)))
    assert(table === expected)
  }

  test("merge test") {
    val table1 = convert(createCodeTree(string2Chars("abc")))
    val table2 = convert(createCodeTree(string2Chars("def")))
    val merged = mergeCodeTables(table1, table2)
    val expected = List(('c', List(0)), ('a', List(1, 0)), ('b', List(1, 1)), ('f', List(0)), ('d', List(1, 0)), ('e', List(1, 1)))
    assert(merged === expected)
  }

  test("quick encode test") {
    val chars = string2Chars("abrakadabra")
    val tree = createCodeTree(chars)
    val charList = string2Chars("abba")
    val encoded = quickEncode(tree)(charList)
    assert(encoded === List(0, 1, 1, 1, 1, 1, 1, 0))
  }

  test("decode french secret") {
    val chars = string2Chars("huffmanestcool")
    val encoded = quickEncode(frenchCode)(chars)
    assert(secret === encoded)
  }
}









