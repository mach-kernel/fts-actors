package actor

import akka.actor.{Actor, ActorLogging, Props}
import util.{TreeNode, TreeOperations}

final case class AddWord(word: String, foundAt: Int)
final case class Query(fragment: String)

object CorpusIndex {
  def props(corpusName: String) = Props(new CorpusIndex(corpusName))
}

class CorpusIndex(val corpusName: String) extends Actor with ActorLogging with TreeOperations[String] {
  private val nonWordRE = "[^\\w]*".r
  var rootNode: Option[TreeNode[String]] = None

  override def preStart() = log.info(s"Index actor for $corpusName up!")
  override def postStop() = log.info(s"Index actor for $corpusName down!")

  override def receive: Receive = {
    case AddWord(word, pos) => {
      val clean = nonWordRE.replaceAllIn(word.toLowerCase, "")

      this.rootNode match {
        case Some(node) => insertTree(node, clean, pos)
        case None => this.rootNode = Some(TreeNode(clean, foundAt = List(pos)))
      }
    }
    case Query(str) => {
      // Reply back with found nodes
      sender() ! str.split(' ')
                    .par
                    .flatMap(w => findItem(rootNode, w.toLowerCase))
                    .toList
    }
    case _ => log.error("Invalid command")
  }
}
