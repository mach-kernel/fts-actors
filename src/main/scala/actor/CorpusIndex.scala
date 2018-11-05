package actor

import akka.actor.{Actor, ActorLogging, Props}
import util.TreeNode
import util.TreeOperations._

final case class AddWord(word: String, foundAt: Int)
final case class Query(fragment: String)

object CorpusIndex {
  def props(corpusName: String) = Props(new CorpusIndex(corpusName))
}

class CorpusIndex(val corpusName: String) extends Actor with ActorLogging {
  private val nonWordRE = "[^\\w]*".r
  var rootNode: TreeNode[String] = TreeNode("")

  override def preStart() = log.info(s"Index actor for $corpusName up!")
  override def postStop() = log.info(s"Index actor for $corpusName down!")

  override def receive: Receive = {
    case AddWord(word, pos) => {
      val clean = nonWordRE.replaceAllIn(word.toLowerCase, "")
      rootNode.insert(clean, pos)
    }
    case Query(str) => {
      // Reply back with found nodes
      sender() ! str.split(' ')
                    .par
                    .flatMap(
                      w => rootNode.findNode(nonWordRE.replaceAllIn(w.toLowerCase, ""))
                    )
                    .toList
    }
    case _ => log.error("Invalid command")
  }
}
