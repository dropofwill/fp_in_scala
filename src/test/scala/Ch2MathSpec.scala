import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.matcher.DataTables
import org.specs2.mutable.Specification

class Ch2MathSpec extends Specification with ScalaCheck with DataTables {
  private val smallPosInt = Gen.choose[Int](2, 100)

  "commutative property for addition" >> {
    prop {
      (a: Int, b: Int) => (b+a) mustEqual(a+b)
    }
  }

  "#abs" >> {
    "never returns a negative number, except MinValue" >> {
      prop {
        a: Int => Math.abs(a) must beGreaterThanOrEqualTo(0).when(a != Int.MinValue)
      }
    }
  }

  "#fib" >> {
    "should pass some hard-coded inputs" in {
      "nth" | "fib" |>
      0 ! 0  |
      1 ! 1  |
      2 ! 1  |
      3 ! 2  |
      4 ! 3  |
      5 ! 5  |
      6 ! 8  |
      7 ! 13 | {
        (a: Int, b: Int) => Math.fib(a) mustEqual b
      }
    }

    "should equal the some of the previous 2 numbers in the sequence" >> {
      prop {
        a: Int => Math.fib(a) mustEqual Math.fib(a-1) + Math.fib(a-2)
      }.setGen(smallPosInt)
    }
  }

  "#isSorted" >> {
    "should pass some hard-coded inputs" in {
      "array" | "sorted?" |>
        Array[Int]()  ! true |
        Array(1,2,3)  ! true |
        Array(2,2,2)  ! true |
        Array(-1,0,1) ! true |
        Array(3,2,1)  ! false |
        Array(3,1,2)  ! false | {
        (arr: Array[Int], isSorted: Boolean) => Math.isSorted(arr) mustEqual isSorted
      }
    }
  }
}
