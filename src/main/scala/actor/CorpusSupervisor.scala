package actor

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.AskSupport
import akka.util.Timeout
import util.{Ranking, TreeNode}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

final case class SupervisorStop()
final case class NewIndex(name: String)
final case class QueryAll(fragment: String)

object CorpusSupervisor {
  def props(): Props = Props(new CorpusSupervisor)
}

class CorpusSupervisor extends Actor with ActorLogging with AskSupport with Ranking {
  implicit val timeout: Timeout = Timeout(5 seconds)

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
      // This is a little meh since we have to rely on mapTo
      // for 'checking' the cast, but it'll do.
      //
      // Can't wait for more progress on this: https://doc.akka.io/docs/akka/current/typed/actors.html
      Future.sequence(indices.par.map(_ ? Query(str)).toList)
        .mapTo[List[List[TreeNode[String]]]]
        .map(indices.zip(_))
        .onComplete {
          case Success(results) => {
            val res = rankResults(results)
            if (res.isEmpty) log.info("No hits found")
            else res.foreach(r => log.info(s"${r.name} (${r.score}): ${r.words}"))
          }
          case Failure(err) => log.error(s"Something went wrong: $err")
        }
    }
    case _ => log.error("Invalid command")
  }
}
