package actor

import akka.actor.{Actor, ActorLogging, Props}
import util.{TreeNode, TreeOperations}

final case class AddWord(word: String, foundAt: Int)
final case class Query(fragment: String)

object CorpusIndex {
  def props(corpusName: String) = Props(new CorpusIndex(corpusName))
}

class CorpusIndex(val corpusName: String) extends Actor with ActorLogging with TreeOperations[String] {
  var rootNode: Option[TreeNode[String]] = None

  override def preStart() = log.info(s"Index actor for $corpusName up!")
  override def postStop() = log.info(s"Index actor for $corpusName down!")

  override def receive: Receive = {
    case AddWord(word, pos) => {
      val lc = word.toLowerCase.replaceAll("[^\\w]*", "")

      this.rootNode match {
        case Some(node) => insertTree(node, lc, pos)
        case None => this.rootNode = Some(TreeNode(lc, foundAt = List(pos)))
      }
    }
    case Query(str) => {
      val found = str.split(' ').par.flatMap(w => findItem(rootNode, w.toLowerCase))
      val present = found.map(n => (n.data, n.foundAt))
      log.info(s"Found: $present")
    }
    case _ => log.error("Invalid command")
  }
}
