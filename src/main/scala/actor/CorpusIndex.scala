package actor

import akka.actor.{Actor, ActorLogging, Props}
import util.{TreeNode, TreeOperations}

final case class AddWord(word: String, foundAt: Int)

object CorpusIndex {
  def props(corpusName: String) = Props(new CorpusIndex(corpusName))
}

class CorpusIndex(val corpusName: String) extends Actor with ActorLogging with TreeOperations[String] {
  var rootNode: Option[TreeNode[String]] = None

  override def preStart() = log.info(s"Index actor for $corpusName up!")
  override def postStop() = log.info(s"Index actor for $corpusName down!")

  override def receive: Receive = {
    case AddWord(word, pos) => {
      val lc = word.toLowerCase

      this.rootNode match {
        case Some(node) => insertTree(node, lc, pos)
        case None => this.rootNode = Some(TreeNode(lc, foundAt = List(pos)))
      }
    }
    case _ => log.error("Invalid command")
  }
}
