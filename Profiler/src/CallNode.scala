package masterthesis
package profiler

import upickle.default.Writer

/** part of a call graph including depth and cycles. */
final case class CallNode(
  function: String,
  depth: Int,
  cycleCount: Long,
  successors: List[CallNode] = Nil,
  globalCalls: Long = 1,
  localCalls: Long = 1
) derives Writer {
  def totalTime: Long =
    cycleCount

  lazy val ownTime: Long =
    totalTime - succesorTime

  lazy val succesorTime: Long =
    successors.map(_.totalTime).sum

  /** Compresses the tree into a list of functions and cycles. */
  def collectSum(mapping: Map[String, (Long, Long)] = Map(), ancestor: String = ""): Map[String, (Long, Long)] = {

    // Update map with this nodes data
    val updatedMap = mapping.updatedWith(function) {
      case Some(c1 -> c2) if function != ancestor => Some(totalTime + c1, ownTime + c2)
      // Handle direct recursions
      case Some(c1 -> c2) if function == ancestor => Some(c1, c2 + ownTime)
      case _                                      => Some(totalTime, ownTime)
    }
    successors.foldLeft(updatedMap)((m, node) => node.collectSum(m, function))
  }

  /** Compresses the tree into a call map. The numbers in the tuple are times called by parent and times called over all. Return
    * value Map[Caller, Map[Calle, (Times called in all executions, Times called in one execution)]]
    */
  def callMap(map: Map[String, Map[String, (Long, Long)]] = Map()): Map[String, Map[String, (Long, Long)]] = {
    val calls = successors.groupBy(_.function).map((func, funcs) =>
      func -> (funcs.map(_.globalCalls).sum, funcs.map(_.localCalls).sum)
    )

    // Update map with this nodes data
    val updatedMap = map.updatedWith(function) {
      case None => Some(calls)
      case Some(map) =>
        Some((map.toSeq ++ calls.toSeq)
          .groupMap(_._1)(_._2)
          .map((func, data) => func -> (data.map(_._1).sum, data.map(_._2).head)))
    }
    successors.foldLeft(updatedMap)((m, node) => node.callMap(m))
  }

  /** Remove nodes which match the given strings. */
  def cutOut(filter: List[String]): CallNode =
    if (filter.nonEmpty) {
      // Partition nodes on given blacklist
      val (keptSuccessors, removedSuccessors) = successors.partition(c => !filter.exists(f => c.function.toLowerCase.contains(f)))
      // Cut all remaining successors
      val cutSuccessors = keptSuccessors.map(_.cutOut(filter))
      // Build new node
      CallNode(
        function,
        depth,
        ownTime + cutSuccessors.map(_.totalTime).sum,
        cutSuccessors
      )
    } else this

  /** Converts this node in a more compact representation */
  def compact: CallNode =
    if (successors.nonEmpty) {
      this.copy(successors = successors.groupBy(_.function).valuesIterator.toList.flatMap {
        // If a functions occurrs more than once, try to compress it
        case nodes if nodes.length > 1 =>
          // Can compress if all nodes have the same path below them
          if (nodes.map(_.showPath).distinct.length == 1) {
            // Merge trees
            nodes.foldLeft(List[CallNode]()) {
              case (Nil, n)          => List(n)
              case (head :: rest, n) => n.merge(head, true) ::: rest
            }
          } else
            nodes
        case nodes => nodes
      })
    } else
      this

  /** Combines two callnodes into one. */
  def merge(other: CallNode, local: Boolean): List[CallNode] =
    // Verify that a merge is possible
    if (showPath == other.showPath)
      List(copy(
        globalCalls = globalCalls + other.globalCalls,
        localCalls = if (local) localCalls + other.localCalls else localCalls,
        cycleCount = cycleCount + other.cycleCount,
        successors = (successors ++ other.successors).groupBy(_.function).values.toList.flatMap {
          case Nil        => Nil
          case List(node) => List(node)
          case nodes => nodes.foldLeft(List[CallNode]()) {
              case (Nil, n)          => List(n)
              case (head :: rest, n) => n.merge(head, false) ::: rest
            }
        }
      ))
    else
      List(this, other)

  override def toString: String =
    s"[${if (localCalls > 1) s"x$localCalls " else ""}$function with $totalTime]"

  /** Show complete tree without cycles. Use to assess safe mergability. */
  def showPath: String =
    s"${" " * depth}[$function]${if (successors.nonEmpty) successors.map(_.showPath).mkString("\n", "\n", "") else ""}"

  /** Show complete tree. */
  def showAll: String =
    s"${" " * depth}$this${if (successors.nonEmpty) successors.map(_.showAll).mkString("\n", "\n", "") else ""}"
}
