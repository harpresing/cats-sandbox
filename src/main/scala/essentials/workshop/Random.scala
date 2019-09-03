package essentials.workshop

import scala.io.StdIn.readLine
import scala.util.{Random, Try}

case class RandomNumberGenerator(start: Int, end: Int) {
  private val random: Random = Random
  def nextInt: Int = start + random.nextInt((end - start) + 1)
  def isInRange(guess: Int): Boolean = guess >= start && guess <= end
}

sealed trait Result
final case class Success(msg: String) extends Result
sealed trait Failure extends Result

final case class NotInRangeError(msg: String) extends Failure
final case class NumberDidNotMatchError(msg: String) extends Failure
final case class NotANumberError(msg: String) extends Failure

object RandomApp extends App {
  println(MoreCleverUtils.randomGeneratorGame)
}

object MoreCleverUtils {

  def randomGeneratorGame: Result = {
    val guess = Utils.readFromUser
    val result = playTheGameAndGetTheResult(guess)

    result match {
      case Left(value) => value
      case Right(value) => value
    }
  }

  def playTheGameAndGetTheResult(guess: String): Either[Failure, Success] =
    for {
      validInput <- validateInput(guess)
      validInRange <- validateIfInRange((gen: RandomNumberGenerator, value: Int) => gen.isInRange(value), validInput)
      matchedGuess <- checkIfGuessMatches(validInRange)
    } yield matchedGuess

  def validateInput(guess: String): Either[Failure, Int] = {
    Try(guess.toInt) match {
      case util.Failure(_) => Left(NotANumberError(s"Input $guess isn't a number"))
      case util.Success(value) => Right(value)
    }
  }
  def validateIfInRange(isInRange: (RandomNumberGenerator, Int) => Boolean, value: Int): Either[Failure, Int] = {
    val randomNumberGenerator = RandomNumberGenerator(1, 10)
    if (isInRange(randomNumberGenerator, value)) Right(value)
    else Left(NotInRangeError(s"Your input number $value is not in range."))
  }

  def checkIfGuessMatches(guess: Int): Either[Failure, Success] = {
    val randomNumber = RandomNumberGenerator(1, 10).nextInt
    if (randomNumber == guess) Right(Success(s"Your guess: $guess matched the randomly generated number $randomNumber"))
    else Left(NumberDidNotMatchError(s"Your guess: $guess did not match the randomly generated number $randomNumber"))
  }
}


// Less Functional
object Utils {

  def playTheGame: Result = checkIfGuessIsInRange(readFromUser)

  def readFromUser: String = {
    println("Please enter a guess between 1 and 10")
    readLine()
  }

  def checkIfGuessIsInRange(guess: String): Result = {
    Try(guess.toInt) match {
      case util.Failure(_) => NotANumberError(s"Input $guess isn't a number")
      case util.Success(value) =>
        val randomNumberGenerator = RandomNumberGenerator(1, 10)
        val randomNumber = randomNumberGenerator.nextInt

        if (!randomNumberGenerator.isInRange(value)) NotInRangeError(s"Your input number $value is not in range.")
        else if (randomNumber == value) Success(s"Your guess: $guess matched the randomly generated number $randomNumber")
        else NumberDidNotMatchError(s"Your guess: $guess did not match the randomly generated number $randomNumber")
    }
  }

}