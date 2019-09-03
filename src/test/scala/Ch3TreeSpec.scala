import org.specs2.ScalaCheck
import org.specs2.matcher.DataTables
import org.specs2.mutable.Specification

class Ch3TreeSpec extends Specification with ScalaCheck with DataTables {
  "#apply" >> {
    "construct some equivalent trees" >> {
      Node(Term, Term, 1) mustEqual Node(Term, Term, 1)
    }
  }

  "#size" >> {
    "simple examples" in {
      "tree" | "res" |>
        Term ! 0 |
        Node(Term, Term, 2) ! 1 |
        Node(Node(Term, Term, 2), Term, 1) ! 2 | {
        (tree: Tree[Int], res: Int) => Tree.size(tree) mustEqual res
      }
    }
  }

  "#max" >> {
    "simple examples" in {
      "tree" | "res" |>
        Term ! 0 |
        Node(Term, Term, 2) ! 2 |
        Node(Node(Term, Term, 3), Term, 1) ! 3 | {
        (tree: Tree[Int], res: Int) => Tree.max(tree) mustEqual res
      }
    }
  }

  "#sum" >> {
    "simple examples" in {
      "tree" | "res" |>
        Term ! 0 |
        Node(Term, Term, 2) ! 2 |
        Node(Node(Term, Term, 3), Term, 1) ! 4 | {
        (tree: Tree[Int], res: Int) => Tree.sum(tree) mustEqual res
      }
    }
  }
}
