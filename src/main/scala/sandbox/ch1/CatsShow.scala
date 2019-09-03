package sandbox.ch1

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

object CatsShow {

  implicit val printableCat: Show[Cat] =
    (cat: Cat) =>
      s"${cat.name.show} " +
        s"is a ${cat.age.show} year old ${cat.color.show} cat."

  def print(cat: Cat): String = cat.show

}
