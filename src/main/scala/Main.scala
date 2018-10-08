import scala.io.{Source, StdIn}
import java.io.File

import actor.{AddWord, CorpusSupervisor, NewIndex, SupervisorStop}
import akka.actor.{ActorRef, ActorSystem, Props}

object Main extends App {
  val actorSystem: ActorSystem = ActorSystem("ftsSystem")
  val supervisor = actorSystem.actorOf(Props[CorpusSupervisor], "ftsSupervisor")

  val helpMessage = "Valid commands are help, exit, read_directory [path], read_file [path]"
  println(helpMessage)

  while (true) {
    val command :: args = StdIn.readLine("fts-actors> ").split(' ').toList

    command.toLowerCase match {
      case "help" => println(helpMessage)
      case "exit" => {
        supervisor ! SupervisorStop
        System.exit(0)
      }
      case "read_directory" => readDirectory(args.head)
      case "read_file" => readFile(args.head)
      case _ => println("Invalid command!")
    }
  }

  /**
    * Read all files from a directory
    * @param path
    */
  def readDirectory(path: String): Unit = {
    val dir = new File(path)

    if (!dir.exists || !dir.isDirectory) {
      println("Invalid directory")
      return
    }

    dir.listFiles.par.foreach(x => readFile(x.getPath))
  }

  def readFile(path: String): Unit = {
    val file = new File(path)

    if (!file.exists || !file.isFile) {
      println("Invalid file")
      return
    }

    supervisor ! NewIndex(file.getName)
    lazy val destinationRef = actorSystem.actorSelection(
      s"user/ftsSupervisor/${file.getName}"
    )

    var pos = 0
    for (l <- Source.fromFile(file).getLines()) {
      l.split(' ').foreach(w => {
        destinationRef ! AddWord(w, pos)
        pos += 1
      })
    }
  }
}