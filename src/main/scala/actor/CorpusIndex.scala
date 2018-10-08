package actor

import akka.actor.{Actor, ActorLogging, Props}

final case class AddWord(word: String, foundAt: Int)

object CorpusIndex {
  def props(corpusName: String) = Props(new CorpusIndex(corpusName))
}

class CorpusIndex(val corpusName: String) extends Actor with ActorLogging {
  override def preStart() = log.info(s"Index actor for $corpusName up!")
  override def postStop() = log.info(s"Index actor for $corpusName down!")

  override def receive: Receive = {
    case AddWord(word, foundAt) => {
      // TODO: indexing
      log.info(s"Hey! $word $foundAt")
    }
    case _ => log.error("Invalid command")
  }
}
