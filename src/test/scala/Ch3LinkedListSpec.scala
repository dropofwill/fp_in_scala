import LinkedList._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.listOf
import org.specs2.ScalaCheck
import org.specs2.matcher.DataTables
import org.specs2.mutable.Specification

import scala.util.Random

class Ch3LinkedListSpec extends Specification with ScalaCheck with DataTables {
  val listOfTrues = listOf(Gen.const(true))
  val listOfFalses = listOf(Gen.const(false))

  val smallerListBiggerList: Gen[(LinkedList[Int], LinkedList[Int])] = for {
    maybeSubList <- Gen.listOf(Arbitrary.arbitrary[Int])
    equalToOrGreaterThanSubListLength <- Gen.chooseNum(
      maybeSubList.size + 1, Integer.MAX_VALUE)
    maybeSuperList <- Gen.listOfN(
      equalToOrGreaterThanSubListLength, Arbitrary.arbitrary[Int])
  } yield (LinkedList(maybeSubList: _*), LinkedList(maybeSuperList: _*))

  val myGen: Gen[(Int, Int)] = for {
    n <- Gen.choose(10,20)
    m <- Gen.choose(2*n, 500)
  } yield (n,m)

  val genSubList: Gen[(List[Int], List[Int])] = for {
    fullList <- Arbitrary.arbitrary[List[Int]]
    sliceBegin: Int <- Gen.chooseNum(0, fullList.size)
    sliceEnd: Int <- Gen.chooseNum(sliceBegin + 1, fullList.size)
    subList: List[Int] <- Gen.const(fullList.slice(sliceBegin, sliceEnd))
  } yield (fullList, subList)

  "#apply" >> {
    "shorthand constructor" >> {
      LinkedList(1,2,3) mustEqual Cons(1, Cons(2, Cons(3, Nil)))
      LinkedList() mustEqual Nil
      LinkedList(Seq(): _*) mustEqual Nil
    }
  }

  "#length" >> {
    "seq length equivalent to linkedlist length" >> {
      prop {
        seq: Seq[Int] =>  seq.length mustEqual len(LinkedList(seq: _*))
      }
    }
  }

  "#sum" >> {
    "sum a list of 1 elements equals that element" >> {
      val emptyInt: LinkedList[Int] = Nil
      sum(emptyInt) mustEqual 0
    }

    "sum a list of 1 elements equals that element" >> {
      prop { a: Int => sum(Cons(a, Nil)) mustEqual a }
      prop { a: BigDecimal => sum(Cons(a, Nil)) mustEqual a }
      prop { a: Byte => sum(Cons(a, Nil)) mustEqual a }
    }

    "sum a list of 2 elements equals the of those 2" >> {
      prop { (a: Int, b: Int) => sum(Cons(a, Cons(b, Nil))) mustEqual a + b }
      prop { (a: BigDecimal, b: BigDecimal) => sum(Cons(a, Cons(b, Nil))) mustEqual a + b }
      // -128 + -1 overflows, but both sides should overflow the same way, for some
      // reason that doesn't happen....
//      prop { (a: Byte, b: Byte) => sum(Cons(a, Cons(b, Nil))) mustEqual a + b }
    }
  }

  "#product" >> {
    "product a list of 1 elements equals that element" >> {
      val emptyInt: LinkedList[Int] = Nil
      product(emptyInt) mustEqual 1
    }

    "product a list of 1 elements equals that element" >> {
      prop { a: Int => product(Cons(a, Nil)) mustEqual a }
      prop { a: BigDecimal => product(Cons(a, Nil)) mustEqual a }
      prop { a: Byte => product(Cons(a, Nil)) mustEqual a }
    }

    "product a list of 2 elements equals the of those 2" >> {
      prop { (a: Int, b: Int) => product(Cons(a, Cons(b, Nil))) mustEqual a * b }
      prop {
        (a: BigDecimal, b: BigDecimal) => product(Cons(a, Cons(b, Nil))) must
          beCloseTo(a * b within 2.significantFigures)
      }
      // -128 + -1 overflows, but both sides should overflow the same way, for some
      // reason that doesn't happen....
//      prop { (a: Byte, b: Byte) => product(Cons(a, Cons(b, Nil))) mustEqual a * b }
    }
  }

  "#tail" >> {
    "removes the head if possible" >> {
      prop {
        (a: Int, b: Seq[Int]) => tail(Cons(a, LinkedList(b: _*))) mustEqual
          LinkedList(b: _*)
      }
    }
  }

  "#setHead" >> {
    "return new list replacing the head" >> {
      prop {
        (a: Int, b: Int, s: Seq[Int]) => setHead(a, Cons(b, LinkedList(s: _*))) mustEqual
          Cons(a, LinkedList(s: _*))
      }
    }
  }

  "#init" >> {
    "un-does append of a single item" >> {
      prop {
        (a: Int, xs: Seq[Int]) => init(append(LinkedList(xs: _*), Cons(a, Nil))) mustEqual
          LinkedList(xs: _*)
      }
    }
  }

  "#reverse" >> {
    "composition is identity" >> {
      prop {
        xs: Seq[Int] => reverse(reverse(LinkedList(xs: _*))) mustEqual
          LinkedList(xs: _*)
      }
    }

    "empty or single item lists unchanged" >> {
      reverse(Nil) mustEqual Nil
      reverse(Cons(1, Nil)) mustEqual Cons(1, Nil)
    }

    "three element lists are reversed" >> {
      prop {
        (a: Int, b: Int, c: Int) => reverse(Cons(a, Cons(b, Cons(c, Nil)))) mustEqual
          Cons(c, Cons(b, Cons(a, Nil)))
      }
    }
  }

  "#flatten" >> {
    "example" >> {
      flatten(LinkedList(LinkedList(1,2,3), Nil, LinkedList(1))) mustEqual
        LinkedList(1,2,3,1)
    }

    "Seq[Seq[a]] => Seq[a]" >> {
      prop {
        (a: Seq[Int], b: Seq[Int]) => flatten(
          LinkedList(LinkedList(a: _*), LinkedList(b: _*))) mustEqual
          append(LinkedList(a: _*), LinkedList(b: _*))
      }
    }
  }

  "#map" >> {
    "add 1 to each" >> {
      LinkedList.map(LinkedList(1,2,3))(_ + 1) mustEqual LinkedList(2,3,4)
    }

    "identity leaves list unchanged" >> {
      prop {
        a: Seq[Int] => LinkedList.map(LinkedList(a: _*))(i => i) mustEqual
          LinkedList(a: _*)
      }
    }
  }

  "flatMap" >> {
    "with identity gives flatten" >> {
      prop {
        (a: Seq[Int], b: Seq[Int]) => {
          val nestedList = LinkedList(LinkedList(a: _*), LinkedList(b: _*))
          flatten(nestedList) mustEqual
          flatMap(nestedList)(identity)
        }
      }
    }
  }

  "#where" >> {
    "only true values remain" >> {
      prop((trues: List[Boolean], falses: List[Boolean]) => {
        val all = LinkedList(Random.shuffle(trues ++ falses): _*)

        LinkedList(trues: _*) mustEqual where(all)(_ == true)
      }).setGens(listOfTrues, listOfFalses)
    }
  }

  "#zipWithSum" >> {
    "length should always equal the longest of the lists" >> {
      prop {
        (l1: Seq[Int], l2: Seq[Int]) => {
          len(zipWithSum(
            LinkedList(l1: _*), LinkedList(l2: _*))(additionSemigroup)) mustEqual
            scala.math.max(l1.size, l2.size)
        }
      }

      "semigroup version consumes all elements from both lists" in {
          "l1"              | "l2"              | "res"             |>
          LinkedList(1,2,3) ! LinkedList(1,2,3) ! LinkedList(2,4,6) |
          LinkedList(1,2)   ! LinkedList(1,2,3) ! LinkedList(2,4,3) |
          LinkedList(1,2,3) ! LinkedList(1,2)   ! LinkedList(2,4,3) |
          Nil               ! LinkedList(1,1,1) ! LinkedList(1,1,1) |
          LinkedList(1,1,1) ! Nil               ! LinkedList(1,1,1) | {
          (l1: LinkedList[Int], l2: LinkedList[Int], res: LinkedList[Int]) =>
            zipWithSum(l1, l2)(additionSemigroup) mustEqual res
        }
      }
    }
  }

  "#zipWithArbitrary" >> {
    "arbitrary version stops once one list is consumed" in {
      "l1"              | "l2"              | "res"             |>
        LinkedList(1,2,3) ! LinkedList(1,2,3) ! LinkedList(2,4,6) |
        LinkedList(1,2)   ! LinkedList(1,2,3) ! LinkedList(2,4)   |
        LinkedList(1,2,3) ! LinkedList(1,2)   ! LinkedList(2,4)   |
        Nil               ! LinkedList(1,1,1) ! Nil               |
        LinkedList(1,1,1) ! Nil               ! Nil               | {
        (l1: LinkedList[Int], l2: LinkedList[Int], res: LinkedList[Int]) =>
          zipWith(l1, l2)(additionSemigroup) mustEqual res
      }
    }
  }

  "#hasSubSeq" >> {
    "simple examples" in {
        "sup"             | "sub"             | "res"  |>
        LinkedList(1,2,3) ! LinkedList(1,2,3) ! true   |
        LinkedList(1,2)   ! LinkedList(1,2,3) ! false  |
        LinkedList(1,2,3) ! LinkedList(1,2)   ! true   |
        Nil               ! LinkedList(1,1,1) ! false  |
        LinkedList(1,1,1) ! Nil               ! false  | {
        (l1: LinkedList[Int], l2: LinkedList[Int], res: Boolean) =>
          hasSubSeq(l1, l2) mustEqual res
      }
    }

    "forall sublists of a list return true" >> {
      prop((bothLists: (List[Int], List[Int])) => {
        bothLists match {
          case (fullList, subList) => hasSubSeq(
            LinkedList(fullList: _*), LinkedList(subList: _*)) mustEqual true
        }
      }).setGen(genSubList)
    }
  }
}
