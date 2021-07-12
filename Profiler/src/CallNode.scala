/** part of a call graph including depth and cycles. */
final case class CallNode(function: String, depth: Int, enterCount: Long, leaveCount: Long, successor: List[CallNode]) {
  def totalTime: Long =
    leaveCount - enterCount

  lazy val ownTime: Long =
    totalTime - succesorTime

  lazy val succesorTime: Long =
    successor.map(_.totalTime).sum

  /** Compresses the tree into a list of functions and cycles. */
  def collectSum(mapping: Map[String, (Long, Long)] = Map(), ancestor: String = ""): Map[String, (Long, Long)] = {

    // Update map with this nodes data
    val updatedMap = mapping.updatedWith(function) {
      case Some(c1 -> c2) if function != ancestor => Some(totalTime + c1, ownTime + c2)
      // Handle direct recursions
      case Some(c1 -> c2) if function == ancestor => Some(c1, c2 + ownTime)
      case _                                      => Some(totalTime, ownTime)
    }
    successor.foldLeft(updatedMap)((m, node) => node.collectSum(m, function))
  }

  /** Compresses the tree into a call map. The numbers in the tuple are times called by parent and times called over all. Return
    * value Map[Caller, Map[Calle, (Times called in all executions, Times called in one execution)]]
    */
  def callMap(map: Map[String, Map[String, (Long, Long)]] = Map(), called: Long = 1): Map[String, Map[String, (Long, Long)]] = {
    val calls = successor.map(_.function).groupBy(identity).map((func, funcs) =>
      func -> (funcs.length.toLong, funcs.length.toLong)
    )

    // Update map with this nodes data
    val updatedMap = map.updatedWith(function) {
      case None => Some(calls)
      case Some(map) =>
        Some((map.toSeq ++ calls.toSeq)
          .groupMap(_._1)(_._2)
          .map((func, data) => func -> (data.map(_._1).sum, data.map(_._2).head)))
    }
    successor.foldLeft(updatedMap)((m, node) => node.callMap(m, updatedMap(function)(node.function)._2))
  }

  /** Remove nodes which match the given strings. */
  def cutOut(filter: List[String]): CallNode = {
    // Partition nodes on given blacklist
    val (keptSuccessors, removedSuccesors) = successor.partition(c => !filter.exists(f => c.function.toLowerCase.contains(f)))
    // Cut all remaining successors
    val cutSuccessors = keptSuccessors.map(_.cutOut(filter))
    // Build new node
    CallNode(
      function,
      depth,
      enterCount,
      // Recalculate the leave count (this breaks its logic, but behaves as expected on the sums)
      enterCount + cutSuccessors.map(_.totalTime).sum + ownTime,
      cutSuccessors
    )
  }

  override def toString: String =
    s"[$function with $totalTime]"

  /** Show complete tree. */
  def showAll: String =
    s"${" " * depth}$this${if (successor.nonEmpty) successor.map(_.showAll).mkString("\n", "\n", "") else ""}"
}