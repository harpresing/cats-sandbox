package sandbox.ch1

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

// This import is essential for implicit instance methods to work
import sandbox.ch1.PrintableInstances._
import sandbox.ch1.PrintableSyntax._

@RunWith(classOf[JUnitRunner])
class PrintableTest extends FunSuite {

  val fatCat: Cat = Cat("Fat Henry", 6, "Black")

  test("Printable should print stuff correctly using generic interface methods") {
    assert(Printable.format(fatCat) === "Fat Henry is a 6 year old Black cat.")
  }

  test("Printable should print stuff correctly using extension methods") {
    // That's what happens in the background when you go fatCat.format
    // new PrintableOps[Cat](fatCat).format
    assert(fatCat.format === "Fat Henry is a 6 year old Black cat.")
    assert(CatsShow.print(fatCat) === "Fat Henry is a 6 year old Black cat.")
    assert(List(1,2,3).format === "1, 2, 3")
  }

}
