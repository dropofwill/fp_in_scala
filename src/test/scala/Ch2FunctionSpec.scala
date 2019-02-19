import org.specs2.ScalaCheck
import org.specs2.matcher.DataTables
import org.specs2.mutable.Specification

class Ch2FunctionSpec extends Specification with ScalaCheck with DataTables {
  private val addOne = (a: Int) => a + 1
  private val subtractFive = (a: Int) => a - 5

  "#curry" >> {
    "curried application is always equivalent to uncurried" >> {
      prop {
        (a: Int, b: Int) =>
          Function.curry(math.max)(a)(b) mustEqual
            math.max(a, b)
      }
    }
  }

  "#uncurry" >> {
    "uncurry • curry is always equivalent to the original function" >> {
      prop {
        (a: Int, b: Int) =>
          Function.uncurry(Function.curry(math.max))(a, b) mustEqual
            math.max(a, b)
      }
    }
  }

  "#compose" >> {
    "compose(f,g) is always equivalent to f(g(_)" >> {
      prop {
        a: Int =>
          addOne(subtractFive(a)) mustEqual
            Function.compose(addOne, subtractFive)(a)
      }
    }
  }
}
