package ru.lester.robot

import scala.util.{Try, Success, Failure}

import scalaz._, Scalaz._
import effect._
import scalaz.stream.io

import com.typesafe.scalalogging.LazyLogging

object Robot extends LazyLogging {

  trait Direction
  case object North extends Direction
  case object South extends Direction
  case object West extends Direction
  case object East extends Direction

  trait Result
  case class ReportResult(res: String) extends Result
  case class DirectionResult(res: Direction) extends Result
  case class PointResult(res: Point) extends Result
  case class PositionResult(res: Position) extends Result

  case class Point(x: Int, y: Int)
  case class Position(place: Point, direction: Direction)


  val LengthOfTheTable = 5

  val UsageErrMsg = """Incorrect usage. Valid commands are:

      PLACE x,y,f
      MOVE
      LEFT
      RIGHT
      REPORT

      where x,y are integers between (0, 5) 
        and f is one of ("North", "West", "South", "East")
  """
  val UnknownCommandErrMsg = "Unknown command"
  val CommandOrderErrMsg = """ First command must be: "PLACE x,y,f" """
  val IncorrectFormatErrMsg = """Incorrect format. Use "PLACE x,y,f" """
  val NotANumberErrMsg = "Either x or y is not a number"
  val UnknownDirectionErrMsg = "Unknown direction"
  val OutOfTheTableErrMsg = "Robot can't move out of the table"

  val positionPlace = Lens.lensu[Position, Point] (
    (a, value) => a.copy(place = value),
    _.place
  )

  val placeX = Lens.lensu[Point, Int] (
    (a, value) => a.copy(x = value),
    _.x
  )

  val placeY = Lens.lensu[Point, Int] (
    (a, value) => a.copy(y = value),
    _.y
  )

  val positionDirection = Lens.lensu[Position, Direction] (
    (a, value) => a.copy(direction = value),
    _.direction
  )

  val positionX = positionPlace >=> placeX
  val positionY = positionPlace >=> placeY

  type StatePosition[+A] = State[Position, A]
  type WriterTStateResult[+A] = WriterT[StatePosition, List[String], A]

  def main(args: Array[String]): Unit = {
  	val fileName = args(0)

    val lns = io.linesR(fileName)

  	val result = processCommands(lns.runLog.run)
    processResults(result).unsafePerformIO()
  }
  
  def processResults(result: String \/ List[Result]): IO[Unit] = {
    IO {
      result.fold(errMsg => println(errMsg), _.map {
        case ReportResult(msg) => println(msg)
        case _ => ()
      })
    }
  }

  def processCommands(commands: Seq[String]): String \/ List[Result] = {
  	val result = commands.headOption.fold[String \/ List[Result]](UsageErrMsg.left) { fc =>
      if(!fc.startsWith("PLACE")) CommandOrderErrMsg.left
  	  else { 
        val actions: List[WriterT[StatePosition, List[String], String \/ Result]] = commands.map(cmd => { 
	        if(cmd.startsWith("PLACE")) processPlaceCmd(cmd)
	        else if(cmd == "MOVE") move
	        else if(cmd == "LEFT") left
	        else if(cmd == "RIGHT") right
	        else if(cmd == "REPORT") report
	        else s"$UnknownCommandErrMsg: $cmd".left.point[WriterTStateResult]
	      }).toList

	      val oneAction: WriterT[StatePosition, List[String], List[String \/ Result]] = actions.sequence[WriterTStateResult, String \/ Result]

        val res: (List[String], List[String \/ Result]) = oneAction.run.eval(Position(Point(0, 0), North)) 

        // Log actions with res._1

        res._2.sequence[({type λ[+α] = String \/ α})#λ, Result]       
  	  }
  	}
  	result  	
  }

  def parseDirection(dir: String): Either[String, Direction] =  { 
  	dir match {
  	  case "North" => Right(North)
  	  case "West" => Right(West)
  	  case "South" => Right(South)
  	  case "East" => Right(East)
  	  case other => Left(s"$UnknownDirectionErrMsg: $other")
    }
  }   

  def processPlaceCmd(placeStr: String): WriterTStateResult[String \/ Result] = {
  	  val placeCmd = placeStr.split(' ')
  	  if(placeCmd.length != 2) IncorrectFormatErrMsg.left.point[WriterTStateResult]
  	  else {
  		  val placeArgs = placeCmd(1).split(',')
  		  if(placeArgs.length != 3) IncorrectFormatErrMsg.left.point[WriterTStateResult]
    	  val xRes = Try(placeArgs(0).toInt)
    	  val yRes = Try(placeArgs(1).toInt)
    	  val res = xRes.flatMap(x => yRes.map(y => (x, y)))
    	  res match {
    	    case Success(xy) => parseDirection(placeArgs(2)).fold(_.left.point[WriterTStateResult], dir => place(xy._1, xy._2, dir))
    	    case Failure(e) => s"$NotANumberErrMsg: ${e.getMessage}".left.point[WriterTStateResult]	
    	  }
  	  }
  }
  
  def place(x: Int, y:Int, dir: Direction): WriterTStateResult[String \/ Result] = {
    WriterT.putWith[StatePosition, List[String], String \/ Result]({
      val pos = Position(Point(x, y), dir)
      State { s => (pos, if(!isValidMovement(x, y)) s"$OutOfTheTableErrMsg".left else PositionResult(pos).right) }
    }) (v => v.fold(err => s"Following error has happened during placing into the new position: $err", r => s"Placing to the following position: $r").pure[List] )
  }
  
  def move: WriterTStateResult[String \/ Result] = {
    WriterT.putWith[StatePosition, List[String], String \/ Result](
      for {
        direction <- positionDirection
        x <- direction match {case West => positionX -= 1; case East => positionX += 1; case _ => positionX += 0} 
        y <- direction match {case South => positionY -= 1; case North => positionY += 1; case _ => positionY += 0}
      } yield if (!isValidMovement(x, y)) s"$OutOfTheTableErrMsg".left else PointResult(Point(x, y)).right
    ) (v => v.fold(err => s"Following error has happened during movement: $err", r => s"Moving to following coordinates in the same direction: $r").pure[List] )
  }


  def left: WriterTStateResult[String \/ Result] = {
    WriterT.putWith[StatePosition, List[String], String \/ Result](
      for {
        newDir <- positionDirection %= {
            case North => West
            case West => South
            case South => East
            case East => North
          }
      } yield DirectionResult(newDir).right
    ) (v => v.fold(err => s"Following error has happened during turning left: $err", r => s"Moving to the following direction: $r").pure[List] )
  }

  def right: WriterTStateResult[String \/ Result] = {
    WriterT.putWith[StatePosition, List[String], String \/ Result](
      for {
        newDir <- positionDirection %= {
            case North => East
            case East => South
            case South => West
            case West => North
        }
      } yield DirectionResult(newDir).right
   ) (v => v.fold(err => s"Following error has happened during turning right: $err", r => s"Moving to the following direction: $r").pure[List] )
   }
   
  
  def report: WriterTStateResult[String \/ Result] = {
    WriterT.putWith[StatePosition, List[String], String \/ Result](
      for {
        direction <- positionDirection
        x <- positionX 
        y <- positionY
      } yield ReportResult(s"Robot is on the table. Current place: ($x,$y), direction: $direction").right
    ) (v => v.fold(err => s"Following error has happened during reporting: $err", r => s"Reporting about current state: $r").pure[List] )
  }
  

  def isValidMovement(x: Int, y: Int): Boolean = {
  	if(x < 0  || y < 0 || x > LengthOfTheTable || y > LengthOfTheTable) false else true	
  }
}