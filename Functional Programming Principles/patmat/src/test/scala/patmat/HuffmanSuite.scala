package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("times works"){
    assert(times(List('a', 'a', 'b')) === List(('a',2), ('b', 1)))
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton"){
    assert(singleton(List(new Leaf('c', 2))))
    assert(!singleton(List(new Leaf('a', 1), new Leaf('b', 2))))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("testing until"){
    val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leafList) === List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e','t'), 3),
      Leaf('x', 4), List('e', 't', 'x'), 7)))
  }

  test("encoding"){
    assert(createCodeTree(List('a','a','c','b','c','a')) ===
      Fork(Leaf('a', 3), Fork(Leaf('b',1), Leaf('c', 2), List('b','c'), 3), List('a','b','c'), 6))
  }

  test("decode"){
    assert(decode(Fork(Leaf('a', 3), Fork(Leaf('b',1), Leaf('c', 2), List('b','c'), 3), List('a','b','c'), 6), List(1,0,0,1,1)) ===
      List('b','a','c')
    )
  }

  test("quickencode"){
    assert(quickEncode(Fork(Leaf('a', 3), Fork(Leaf('b',1), Leaf('c', 2), List('b','c'), 3), List('a','b','c'), 6))(List('b','a','c')) ===
      List(1,0,0,1,1))
  }
}
