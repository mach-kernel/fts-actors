package actor

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

final case class SupervisorStop()
final case class NewIndex(name: String)
final case class QueryAll(fragment: String)

object CorpusSupervisor {
  def props(): Props = Props(new CorpusSupervisor)
}

class CorpusSupervisor extends Actor with ActorLogging {
  var indices: List[ActorRef] = List()

  override def preStart(): Unit = log.info(s"${self.path.name} up!")
  override def postStop(): Unit = log.info(s"${self.path.name} down!")

  override def receive: Receive = {
    case SupervisorStop => {
      log.info("Goodbye!")
      context.stop(self)
    }
    case NewIndex(name) => {
      indices = context.actorOf(CorpusIndex.props(name), name = name) :: indices
    }
    case QueryAll(str) => {
      indices.par.foreach(_ ! Query(str))
    }
    case _ => log.error("Invalid command")
  }
}
