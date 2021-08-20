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
def reportStatus(msg: String, reporter: String = "Profiler") =
  putStrLn(Console.BLUE + reporter + Console.RESET + ": " + msg).ignore

/** Prints a blue status message to stdout. */
def reportUpdatedStatus(msg: String, reporter: String = "Profiler") =
  putStr("\r\u001b[K" + Console.BLUE + reporter + Console.RESET + ": " + msg).ignore

/** Prints a red error message to stdout. */
def reportError(msg: String, reporter: String = "Profiler") =
  putStrLn(Console.RED + reporter + Console.RESET + ": " + msg).ignore

/** Prints a green success message to stdout. */
def reportSuccess(msg: String, reporter: String = "Profiler") =
  putStrLn(Console.GREEN + reporter + Console.RESET + ": " + msg).ignore

/** Writes the given string into the given file. Currying for partial application. */
def writeToFile(fileName: String)(data: String): Task[Unit] =
  IO.effect(File(fileName).createFileIfNotExists(true).write(data)).discard

def readFromFile(fileName: String): Task[Iterator[String]] =
  IO.effect(File(fileName).lineIterator)

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

enum TaskState {
  case Initial, Building, ProfilingReady, Profiling, AnalysisReady, Analysing, Finished, Failed
}

type ->[+A, +B] = Tuple2[A, B]
