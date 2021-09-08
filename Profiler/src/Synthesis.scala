package masterthesis
package profiler

import zio.{ZIO}
import zio.duration.*
import zio.clock.Clock
import zio.blocking.Blocking

object Synthesis {

  /** Start synthesis for the given cores. */
  def requestSynthesis(cores: List[String]): ZIO[Blocking & Clock, String, Unit] =
    ZIO.unit

  /** Retrieves synthesis results for the given core hashes. */
  def requestSynthesisResults(hashes: List[String]): ZIO[Blocking, String, Map[String, ((Double, Int), (Double, Int))]] =
    ZIO.succeed(Map())

}
