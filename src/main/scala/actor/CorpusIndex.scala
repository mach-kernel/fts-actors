package actor

import akka.actor.{Actor, ActorLogging, Props}

final case class AddWord(word: String, foundAt: Int)

final case class TreeNode[T](
  data: T,
  var left: Option[TreeNode[T]] = None,
  var right: Option[TreeNode[T]] = None,
  var foundAt: List[Int] = List()
)

object CorpusIndex {
  def props(corpusName: String) = Props(new CorpusIndex(corpusName))
}

class CorpusIndex(val corpusName: String) extends Actor with ActorLogging {
  var rootNode: Option[TreeNode[String]] = None

  override def preStart() = log.info(s"Index actor for $corpusName up!")
  override def postStop() = log.info(s"Index actor for $corpusName down!")

  override def receive: Receive = {
    case AddWord(word, foundAt) => {
      val lc = word.toLowerCase

      this.rootNode match {
        case Some(node: TreeNode[String]) => insertTree(node, lc, foundAt)
        case None => this.rootNode = Some(TreeNode(lc, foundAt = List(foundAt)))
      }
    }
    case _ => log.error("Invalid command")
  }

  /**
    * Tail recursive tree insert
    *
    * @param node
    * @param word
    * @param foundAt
    */
  private def insertTree(node: TreeNode[String], word: String, foundAt: Int): Unit = {
    if (word == node.data) {
      node.foundAt = foundAt :: node.foundAt
      return
    }

    if (word > node.data) {
      node.right match {
        case Some(child) => insertTree(child, word, foundAt)
        case None => node.right = Some(TreeNode(word, foundAt = List(foundAt)))
      }
    } else {
      node.left match {
        case Some(child) => insertTree(child, word, foundAt)
        case None => node.left = Some(TreeNode(word, foundAt = List(foundAt)))
      }
    }
  }
}
