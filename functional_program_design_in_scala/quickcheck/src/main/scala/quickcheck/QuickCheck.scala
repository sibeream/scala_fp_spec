package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll
import scala.math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] =
    for
      x <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    yield
      insert(x, h)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    findMin(insert(a, empty)) == a
  }

  property("two_elem_empty") = forAll { (x: Int, y:Int) =>
    val min = if x < y then x else y
    findMin(insert(x, (insert(y, empty)))) == min
  }

  property("empty_empty") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("sorted") = forAll { (h: H) =>
    isSorted(findMin(h), h)
  }

  def isSorted(a: Int, h: H): Boolean =
    if isEmpty(h) then true
    else
      val min = findMin(h)
      if a > min then false
      else isSorted(min, deleteMin(h))

  property("meld_min") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  property("equals") = forAll { (h1: H, h2: H) =>
    heapsEquals(
      meld(h1, h2),
      meld(
        deleteMin(h1),
        insert(findMin(h1), h2)))
  }

  def heapsEquals(h1: H, h2: H): Boolean =
    if isEmpty(h1) && isEmpty(h2) then true
    else if isEmpty(h1) && !isEmpty(h2) then false
    else if !isEmpty(h1) && isEmpty(h2) then false
    else
      findMin(h1) == findMin(h2) &&
        heapsEquals(deleteMin(h1), deleteMin(h2))
