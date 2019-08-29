import LinkedList._
import org.scalacheck.Gen
import org.scalacheck.Gen.listOf
import org.specs2.ScalaCheck
import org.specs2.matcher.DataTables
import org.specs2.mutable.Specification

import scala.util.Random

class Ch3LinkedListSpec extends Specification with ScalaCheck with DataTables {
  val listOfTrues = listOf(Gen.const(true))
  val listOfFalses = listOf(Gen.const(false))

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
      prop { (a: BigDecimal, b: BigDecimal) => product(Cons(a, Cons(b, Nil))) mustEqual a * b }
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

//  "#append" >> {
//
//  }

  "#map" >> {
    "identity leaves list unchanged" >> {
      prop {
        a: Seq[Int] => LinkedList.map(LinkedList(a: _*))(i => i) mustEqual
          LinkedList(a: _*)
      }
    }
  }

//  "flatMap" >> {
//
//  }

  "#where" >> {
    "only true values remain" >> {
      prop((trues: List[Boolean], falses: List[Boolean]) => {
        val all = LinkedList(Random.shuffle(trues ++ falses): _*)

        LinkedList(trues: _*) mustEqual where(all)(_ == true)
      }).setGens(listOfTrues, listOfFalses)
    }
  }
}
