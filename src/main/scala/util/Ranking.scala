package util

import akka.actor.ActorRef

final case class RankedBody(name: String, words: List[String], score: Double)
final case class WordInstance(word: String, pos: Int)

trait Ranking {
  /**
    * Apply ranking and return results.
    *
    * @param results
    */
  protected def rankResults(results: List[(ActorRef, List[TreeNode[String]])]): List[RankedBody] = {
    results.collect {
      case (ref, nodes@_ :: _) =>
        RankedBody(ref.path.name, nodes.map(_.data), score(nodes))
    }.sortBy(r => -r.score)
  }

  /**
    * Score a list of results. Each word found counts for 0.001,
    * each word found in a phrase entered in the same order as the
    * user counts for 100 points.
    *
    * @param hits
    * @return
    */
  private def score(hits: List[TreeNode[String]]): Double = {
    val baseScore = hits.map(_.foundAt.length).sum * 0.001

    var lastWordInstance :: sorted =
      hits.flatMap(h => h.foundAt.map(p => WordInstance(h.data, p)))
          .sortBy(_.pos)

    var longestSubSeq, curSubSeq = 0
    sorted.foreach { s =>
      if ((s.word != lastWordInstance.word) &&
           Math.abs(s.pos - lastWordInstance.pos) == 1) {
        curSubSeq += 1
      } else {
        curSubSeq = 0
      }

      lastWordInstance = s
      if (curSubSeq > longestSubSeq) longestSubSeq = curSubSeq
    }

    baseScore + (longestSubSeq * 100)
  }
}
