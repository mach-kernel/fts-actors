package actor

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.AskSupport
import akka.util.Timeout
import util.TreeNode

import scala.collection.mutable.ListBuffer
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

class CorpusSupervisor extends Actor with ActorLogging with AskSupport {
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
          case Success(results) => processResults(results)
          case Failure(err) => log.error(s"Something went wrong: $err")
        }
    }
    case _ => log.error("Invalid command")
  }

  /**
    * Filter out empty data and print out hits with scores.
    *
    * @param results
    */
  private def processResults(results: List[(ActorRef, List[TreeNode[String]])]): Unit = {
    val present = results.filter(_._2.nonEmpty)
    if (present.isEmpty) {
      log.info("No hits found!")
      return
    }

    // TODO: sort before display but fine for now
    present.foreach { case (a, hits) => {
      log.info(s"${a.path.name}: search score ${score(hits)} (${hits.map(_.data)})")
    } }
  }

  /**
    * Score a list of results. Each word occurrence counts for 1 point,
    * and then the sum of words that can be sequenced (i.e. a phrase)
    * counts for 0.25 points.
    *
    * TODO: sequence bit
    *
    * Major assumption that user types query fragment in order.
    *
    * @param hits
    * @return
    */
  private def score(hits: List[TreeNode[String]]): Double = {
    hits.map(_.foundAt.length).sum + 0.0
  }
}
