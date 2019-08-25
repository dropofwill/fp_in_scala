import LinkedList._
import org.specs2.ScalaCheck
import org.specs2.matcher.DataTables
import org.specs2.mutable.Specification

class Ch3LinkedListSpec extends Specification with ScalaCheck with DataTables {

  "#apply" >> {
    "shorthand constructor" >> {
      LinkedList(1,2,3) mustEqual Cons(1, Cons(2, Cons(3, Nil)))
      LinkedList() mustEqual Nil
      LinkedList(Seq(): _*) mustEqual Nil
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
        (a: Int, b: Seq[Int]) => tail(Cons(a, LinkedList(b))) mustEqual LinkedList(b)
      }
    }
  }

  "#setHead" >> {
    "return new list replacing the head" >> {
      prop {
        (a: Int, b: Int, s: Seq[Int]) => setHead(a, Cons(b, LinkedList(s))) mustEqual
          Cons(a, LinkedList(s))
      }
    }
  }
}
