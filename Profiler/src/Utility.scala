package masterthesis
package profiler

import java.io.IOException
import java.io.{File => JFile}

import better.files.File

import zio.*
import zio.console.{Console => ZConsole, putStrLn, putStr}
import zio.blocking.{effectBlocking, blocking, Blocking}
import zio.process.{Command, ProcessOutput, CommandError, Process}

/** Prints a blue status message to stdout. */
def reportStatus(reporter: String)(msg: String) =
  ZIO.collectAll(msg.split("\n").map(s => putStrLn(Console.BLUE + reporter + Console.RESET + ": " + s))).ignore

/** Prints a blue status message to stdout. */
def reportUpdatedStatus(reporter: String)(msg: String) =
  putStr("\r\u001b[K" + Console.BLUE + reporter + Console.RESET + ": " + msg).ignore

/** Prints a red error message to stdout. */
def reportError(reporter: String)(msg: String) =
  putStrLn(Console.RED + reporter + Console.RESET + ": " + msg).ignore

/** Prints a green success message to stdout. */
def reportSuccess(reporter: String)(msg: String) =
  putStrLn(Console.GREEN + reporter + Console.RESET + ": " + msg).ignore

/** Writes the given string into the given file. Currying for partial application. */
def writeToFile(fileName: String)(data: String): Task[Unit] =
  IO.effect(File(fileName).createFileIfNotExists(true).writeBytes(data.iterator.map(_.toByte))).discard

/** Reads a given file and streams contents as a line iterator. */
def readFromFile(fileName: String): Task[Iterator[String]] =
  IO.effect(File(fileName).lineIterator)

  /** Reads a given file and returns all content as a single string. */
  def readAllFromFile(fileName: String): Task[String] =
    IO.effect(File(fileName).contentAsString)

/** Runs the given command and returns a tuple of (exit code, stdout, stderr). */
def runForReturn(args: String*): ZIO[Blocking, CommandError, (Int, String, String)] = for {
  proc <- Command(args.head, args.tail.filter(!_.isBlank)*).run
  code <- proc.exitCode
  stdout <- proc.stdout.string
  stderr <- proc.stderr.string
} yield (code.code, stdout, stderr)

/** Runs a shell command without any return wanted. */
def runForEffect(args: String*): ZIO[Blocking, Throwable, Unit] =
  Command(args.head, args.tail.filter(!_.isBlank)*).exitCode.discard

/** Runs a shell command with the output written into the given file. */
def runForFileOutput(logFile: String, mergeErrors: Boolean = false)(args: String*): ZIO[Blocking, CommandError, Unit] =
  Command(args.head, args.tail.filter(!_.isBlank)*).stdout(
    ProcessOutput.FileRedirect(new JFile(logFile))
  ).exitCode.discard

enum TaskState derives CanEqual {
  case Initial, Preflight, Building, ProfilingReady, Profiling, AnalysisReady, Analysing, Finished, Failed
}

class CountDownLatch[E] private (counter: Ref[Int], promise: Promise[E, Unit]) {

  /** Counts one down and waits for completion */
  def await = for {
    count <- counter.updateAndGet(_ - 1)
    _ <- if (count > 0) promise.await else promise.succeed(())
  } yield ()

  /** Counts one down and waits for completion. Additionally, the fiber hitting zero performs a given effect before releasing. */
  def awaitForEffect[R](e: ZIO[R, E, Any]) = for {
    count <- counter.updateAndGet(_ - 1)
    _ <-
      if (count > 0) promise.await
      else for {
        out <- e.either
        _ <- out match {
          case Right(_) =>
            promise.succeed(())
          case Left(e) => promise.fail(e)
        }
      } yield ()
  } yield ()
}

object CountDownLatch {
  def make[E](number: Int) = for {
    counter <- Ref.make(number)
    promise <- Promise.make[E, Unit]
  } yield new CountDownLatch[E](counter, promise)

}

type ->[+A, +B] = Tuple2[A, B]
