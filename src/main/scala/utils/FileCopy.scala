package utils

import java.io._
import java.nio.file.Files

import cats.effect.concurrent.Semaphore
import cats.effect._
import cats.implicits._

/**
 * See [[https://typelevel.org/cats-effect/tutorial/tutorial.html#introduction this]].
 */
object FileCopy extends IOApp {


  override def run(args: List[String]): IO[ExitCode] =
    for {
      _    <- if(args.length < 2) IO.raiseError(new IllegalArgumentException("Need origin and destination files"))
              else IO.unit
      origin = new File(args.head)
      destination = new File(args(1))
      _     <- validateFiles(origin, destination)
      _     <- checkIfFileExists(destination)
      count <- copy(origin, destination)
      _     <- IO(println(s"$count bytes copied from ${origin.getPath} to ${destination.getPath}"))
    } yield ExitCode.Success

  val readLn = IO(scala.io.StdIn.readLine)

  def putStrlLn(value: String): IO[Unit] = IO(println(value))

  private def checkIfFileExists(destination: File): IO[Unit] =
    if (destination.exists())
      for {
      _  <- putStrlLn(s"The destination file ${destination.getPath} exists and will be overwritten. Do you want to continue? (y/n)")
      choice <- readLn
      _ <- validateChoice(choice)
    } yield ()
   else IO.unit

  private def validateChoice(choice: String): IO[Unit] =
    if (choice.equalsIgnoreCase("y")) IO.unit
    else if (choice.equalsIgnoreCase("n"))  IO.raiseError(new IllegalArgumentException("File cannot be overwritten, exiting"))
    else IO.raiseError(new IllegalArgumentException("Invalid choice, please enter either y or n"))

  private def validateFiles(origin: File, destination: File): IO[Unit] =
    if (origin.equals(destination)) IO.raiseError(new IllegalArgumentException("Origin and destination files cannot be the same."))
    else if (!Files.isReadable(origin.toPath)) IO.raiseError(new IllegalArgumentException("Origin file is not readable."))
    else if (!Files.isWritable(destination.toPath)) IO.raiseError(new IllegalArgumentException("Destination file is not writable."))
    else IO.unit

  def copy(origin: File, destination: File)(implicit concurrent: Concurrent[IO]): IO[Long] = {
    for {
      guard <- Semaphore[IO](1) // In case of the user cancelling the copy
      // use returns IO instances that are cancelable
      count <- inputOutputStreams(origin, destination, guard).use {
        // Before calling to transfer we acquire the semaphore, which is not released until transfer is done.
      case (in, out) => guard.withPermit(transfer(in, out))
    }
  } yield count
  }

  /**
   * Loop that at each iteration reads data from the input stream into a buffer
   * and then writes the buffer contents into the output stream.
   */
  private def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
    for {
      buffer <- IO(new Array[Byte](1024 * 10))
      total <- transmit(origin, destination, buffer, 0L)
    } yield total

  private def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
    for {
      amount <- IO(origin.read(buffer, 0, buffer.length))
      count <- if(amount > -1) IO(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
      else IO.pure(acc)
    } yield count

  private def inputOutputStreams(in: File, out: File, guard: Semaphore[IO]): Resource[IO, (InputStream, OutputStream)] = {
    for {
      inputStr <- inputStream(in, guard)
      outputStr <- outputStream(out, guard)
    } yield (inputStr, outputStr)
  }

  private def inputStream(f: File, guard: Semaphore[IO]): Resource[IO, FileInputStream] = Resource.make {
    IO(new FileInputStream(f))
  }{
        // Just swallowing the error here but it should at least be logged
    inputStream => guard.withPermit {
      IO(inputStream.close()).handleErrorWith(_ => IO.unit)
    }
  }

  private def outputStream(f: File, guard: Semaphore[IO]): Resource[IO, FileOutputStream] =
    Resource.make(IO(new FileOutputStream(f)))(outputStream => guard.withPermit(IO(outputStream.close()).handleErrorWith(_ => IO.unit)))
}
