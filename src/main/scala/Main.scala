import scala.io.{Source, StdIn}
import java.io.File
import java.util.logging.Logger

import actor._
import akka.actor.{ActorSystem, Props}

object Main extends App {
  val logger = Logger.getLogger(getClass.getName)
  val actorSystem: ActorSystem = ActorSystem("ftsSystem")
  val supervisor = actorSystem.actorOf(Props[CorpusSupervisor], "ftsSupervisor")

  val helpMessage = "Valid commands are help, read_directory [path], read_file [path]. ^C to exit."
  println(helpMessage)

  while (true) {
    val command :: args = StdIn.readLine("fts-actors> ").split(' ').toList

    command.toLowerCase match {
      case "help" => println(helpMessage)
      case "read_directory" => loadDirectory(args.head)
      case "read_file" => loadFile(args.head)
      case "find" => supervisor ! QueryAll(args.mkString(" "))
      case "\n" => // no-op
      case _ => println("Invalid command!")
    }
  }

  /**
    * Load all files from path
    * @param path
    */
  def loadDirectory(path: String): Unit = {
    val dir = new File(path)

    if (!dir.exists || !dir.isDirectory) {
      println("Invalid directory")
      return
    }

    dir.listFiles.par.foreach(x => loadFile(x.getPath))
  }

  /**
    * Create an index, tokenize, and load from a file
    * @param path
    */
  def loadFile(path: String): Unit = {
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

    logger.info(s"Index for ${file.getName} complete!")
  }
}