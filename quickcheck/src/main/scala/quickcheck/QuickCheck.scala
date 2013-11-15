package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import java.util.NoSuchElementException
import scala.collection.mutable.ArrayBuffer

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("delete min") = forAll { (h: H) =>
    (isEmpty(h) == false) ==> {
      val min = findMin(h)
      val h2 = deleteMin(h)
      isEmpty(h2) match {
        case true   => true
        case false  => findMin(h2) >= min
      }
    }
  }

  property("deleteMin returns elements in correct order") = forAll { list: List[Int] =>
    val h = list.foldLeft(empty)((ret, e) => insert(e, ret))

    // turn mins of h into Stream[A]
    def stream(h: H): Stream[A] = {
      isEmpty(h) match {
        case true => Stream.empty
        case false => Stream.cons(findMin(h), stream(deleteMin(h)))
      }
    }
    list.sorted == stream(h).toList
  }

  property("meld heap will return the min of two heaps.") = forAll { (h: H, h2: H) =>
    (isEmpty(h) == false && isEmpty(h2) == false) ==> {
      val min = findMin(h)
      val min2 = findMin(h2)
      if (min > min2) {
        findMin(meld(h, h2)) == min2
      } else {
        findMin(meld(h, h2)) == min
      }
    }
  }

  property("meld heap will return the min of two heaps.") = forAll { (h: H) =>
    (isEmpty(h) == false) ==> {
      val min = findMin(h)
      findMin(meld(h, empty)) == min
    }
  }

  lazy val genHeap: Gen[H] = for {
    list <- arbitrary[List[Int]]
  } yield list.foldLeft(empty)((ret, e) => insert(e, ret))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


}
