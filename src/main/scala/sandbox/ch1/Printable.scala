package sandbox.ch1

// This is a type class
trait Printable[A] {
  def format(value: A): String
}

final case class Cat(name: String, age: Int, color: String)

// We define type instances by creating concrete
// implementations of the type class and tagging them as implicit
object PrintableInstances {
  implicit val printableInt: Printable[Int] = (value: Int) => String.valueOf(value)

  implicit val printableString: Printable[String] = (value: String) => value

  implicit val printableList: Printable[List[Int]]
  = (value: List[Int]) =>
    value.foldLeft("")((acc: String, b: Int) => {
      (acc, b) match {
        case ("", b) => acc + b
        case _ => acc + ", " + b
      }
    } )

  implicit val printableCat: Printable[Cat] =
    (cat: Cat) =>
      s"${Printable.format(cat.name)} " +
        s"is a ${Printable.format(cat.age)} year old ${Printable.format(cat.color)} cat."

}

// Companion object (Singleton, all methods are static)
object Printable {
  // Example of a generic interface method
  def format[A](value: A)(implicit printable: Printable[A]): String =
    printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]): Unit =
    println(format(value))
}

// Extension methods: Used to extend existing types with interface methods
// Cats refers to this as "syntax" for type classes
object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String =
      printable.format(value)
    def print(implicit printable: Printable[A]): Unit =
      println(format)
  }
}