package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

//  lazy val genHeap: Gen[H] = oneOf(
//    const(empty),
//    for{
//      k <- arbitrary[A]
//      h <- genHeap
//    }yield insert(k, h)
//  )

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  lazy val genNonEmptyHeap: Gen[H] = for{
    k <- arbitrary[A]
    h <- genHeap
  }yield  insert(k, h)

  lazy val genEmptyHeap: Gen[H] = const(empty)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll(genHeap) { h =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOf2") = forAll { (a: Int, b: Int) =>
      val h = insert(a, insert(b, empty))
      findMin(h) == math.min(a, b)
  }

  property("deleteUniqueMin") = forAll{ (i: Int) => isEmpty(deleteMin(insert(i, empty)))}

  property("sorted") = forAll(genHeap){
    h => {
            def checkSorted(h: H, prev: Int): Boolean = h match {
              case List() => true
              case _ =>
                if (findMin(h) >= prev) checkSorted(deleteMin(h), findMin(h))
                else false
            }

            if(!isEmpty(h)) checkSorted(h, findMin(h))
            else checkSorted(h, 0)
    }
  }

  property("minOf2MeldingHeaps") = forAll (genNonEmptyHeap, genNonEmptyHeap){ (h1, h2) =>
    val melded: H = meld(h1, h2)
    findMin(melded) == math.min(findMin(h1), findMin(h2))
  }

  property("minOf2MeldingEmptyHeaps") = forAll (genEmptyHeap, genEmptyHeap){ (h1, h2) =>
    val melded: H = meld(h1, h2)
    isEmpty(melded)
  }

  property("minOf2MeldingHeaps1stEmpty") = forAll (genEmptyHeap, genNonEmptyHeap){ (h1, h2) =>
    val melded: H = meld(h1, h2)
    findMin(melded) == findMin(h2)
  }

  property("minOf2MeldingHeaps1stEmpty") = forAll (genNonEmptyHeap, genEmptyHeap){ (h1, h2) =>
    val melded: H = meld(h1, h2)
    findMin(melded) == findMin(h1)
  }

  property("insertingNewMin") = forAll(genNonEmptyHeap) { (h) => {
    val m = findMin(h)
    val h1 = deleteMin(h)
    findMin(insert(m, h1)) == m
  }
  }

  property("melding") = forAll(genNonEmptyHeap, genNonEmptyHeap) {(h1, h2) =>
    val h12 = meld(h1, h2)
    val h21 = meld(h2, h1)

    def equal(h1: H, h2: H): Boolean = {
      (h1, h2) match {
        case(List(), List()) => true
        case (List(), _) => false
        case (_, List()) => false
        case (_, _) => findMin(h1) == findMin(h2) && equal(deleteMin(h1), deleteMin(h2))
      }
    }

    def insertMeld(h1: H, h2: H): H = (h1, h2) match {
      case(_, List()) => h1
      case (_, _) => insertMeld(insert(findMin(h2), h1), deleteMin(h2))
    }

    equal(h12, h21) && equal(h12, insertMeld(h1, h2))
  }

}
