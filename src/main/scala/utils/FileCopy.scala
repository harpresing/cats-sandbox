package utils

import java.io.{File, FileInputStream, FileOutputStream, InputStream, OutputStream}

import cats.effect.{IO, Resource}

/**
 * See [[https://typelevel.org/cats-effect/tutorial/tutorial.html#introduction this]].
 */
case class FileCopy() {

  def copy(origin: File, destination: File): IO[Long] =
    inputOutputStreams(origin, destination).use {
      case (in, out) => transfer(in, out)
    }

  private def transfer(origin: InputStream, destination: OutputStream): IO[Long] = ???

  private def inputOutputStreams(in: File, out: File): Resource[IO, (InputStream, OutputStream)] = {
    for {
      inputStr <- inputStream(in)
      outputStr <- outputStream(out)
    } yield (inputStr, outputStr)
  }

  private def inputStream(f: File): Resource[IO, FileInputStream] = Resource.make {
    IO(new FileInputStream(f))
  }{
        // Just swallowing the error here but it should at least be logged
    inputStream => IO(inputStream.close()).handleErrorWith(_ => IO.unit)
  }

  private def outputStream(f: File): Resource[IO, FileOutputStream] =
    Resource.make(IO(new FileOutputStream(f)))(outputStream => IO(outputStream.close()).handleErrorWith(_ => IO.unit))
}
