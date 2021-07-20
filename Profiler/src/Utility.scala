package masterthesis
package profiler

import java.io.IOException

import better.files.File

import zio.*
import zio.console.{Console => ZConsole, putStrLn}
import zio.blocking.{effectBlocking, blocking, Blocking}

/** Prints a blue status message to stdout. */
def reportStatus(msg: String, reporter: String = "Profiler") =
  putStrLn(Console.BLUE + reporter + Console.RESET + ": " + msg).ignore

/** Prints a red error message to stdout. */
def reportError(msg: String, reporter: String = "Profiler") =
  putStrLn(Console.RED + reporter + Console.RESET + ": " + msg).ignore

/** Prints a green success message to stdout. */
def reportSuccess(msg: String, reporter: String = "Profiler") =
  putStrLn(Console.GREEN + reporter + Console.RESET + ": " + msg).ignore

/** Writes the given string into the given file. Currying for partial application. */
def writeToFile(fileName: String)(data: String): Task[Unit] =
  IO.effect(File(fileName).write(data)).discard

def readFromFile(fileName: String): Task[Iterator[String]] =
  IO.effect(File(fileName).lineIterator)

def runForReturn(args: os.Shellable*): URIO[Blocking, os.CommandResult] =
  blocking {
    ZIO.effectTotal(os.proc(args*).call(check = false, mergeErrIntoOut = true))
  }

/** Runs a shell command without any return wanted. */
def runForEffect(args: os.Shellable*): ZIO[Blocking, Throwable, Unit] =
  effectBlocking {
    os.proc(args*).call()
  }.discard

/** Runs a shell command with the output written into the given file. */
def runForFileOutput(logFile: os.Path, mergeErrors: Boolean = false)(args: os.Shellable*): ZIO[Blocking, Throwable, Unit] =
  effectBlocking {
    os.proc(args*).call(stdout = logFile, mergeErrIntoOut = mergeErrors)
  }.discard

type ->[+A, +B] = Tuple2[A, B]
