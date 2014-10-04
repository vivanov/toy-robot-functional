package ru.lester.robot

import org.specs2.mutable._

import com.typesafe.scalalogging.LazyLogging

import Robot._

class RobotSpec extends Specification with LazyLogging  {
  "Empty command sequence for robot" should {
 	  "return incorrect usage error message" in {
 		  val res = Robot.processCommands(Nil).toEither
 		  res must beLeft[String]
 		  res.left.get must beEqualTo(UsageErrMsg)
 	  }
  }

  "Any command that contains is not known for robot" should {
    "return error message about unknown command" in {
    	val commands = List("PLACE 0,0,West", "MOVIE", "RIGHT", "REPORT")
 		  val res = Robot.processCommands(commands).toEither
 		  res must beLeft[String]
 		  res.left.get.startsWith(UnknownCommandErrMsg) must beTrue
    }
  }

  "Any command sequence that doesn't start with PLACE command" should {
    "return error message about command order" in {
    	val commands = List("MOVE", "RIGHT", "REPORT")
 		  val res = Robot.processCommands(commands).toEither
 		  res must beLeft[String]
 		  res.left.get must beEqualTo(CommandOrderErrMsg)
    }
  }

  "Any PLACE command that doesn't contain arguments" should {
    "return error message about incorrect format" in {
    	val commands = List("PLACE", "MOVE", "RIGHT", "REPORT")
 		  val res = Robot.processCommands(commands).toEither
 		  res must beLeft[String]
 		  res.left.get must beEqualTo(IncorrectFormatErrMsg)
    }
  }

  "Any PLACE command that contains non-integer positional arguments" should {
    "return not a number error message" in {
    	val commands = List("PLACE a,0,North", "MOVE", "RIGHT", "REPORT")
 		  val res = Robot.processCommands(commands).toEither
 		  res must beLeft[String]
 		  res.left.get.startsWith(NotANumberErrMsg) must beTrue
    }
  }  

  "Any PLACE command that contians unknown direction argument" should {
    "return unknown direction error message" in {
  	  val commands = List("PLACE 0,0,ggg", "MOVE", "RIGHT", "REPORT")
 		  val res = Robot.processCommands(commands).toEither
 		  res must beLeft[String]
 		  res.left.get.startsWith(UnknownDirectionErrMsg) must beTrue
    }
  }

  "Any PLACE command that makes attempt to move robot out of the table" should {
    "return error message from robot that he can't move in this position" in {
  	  val commands = List("PLACE 0,0,West", "MOVE", "RIGHT", "REPORT")
 		  val res = Robot.processCommands(commands).toEither
 		  res must beLeft[String]
 		  res.left.get must beEqualTo(OutOfTheTableErrMsg)
    }
  }

  "Any sequence of commands that tries to move robot out of the table" should {
    "return error message from robot that he can't move in this position" in {
  	  val commands = List("PLACE 3,3,East", "MOVE", "MOVE", "MOVE", "REPORT")
 		  val res = Robot.processCommands(commands).toEither
 		  res must beLeft[String]
 		  res.left.get must beEqualTo(OutOfTheTableErrMsg)
    }
  }

  s"""Sequence of commands: "PLACE 0,1,North", "MOVE", "RIGHT", "REPORT"""" should {
    "return new list of corresponding results and robot located in (0, 2) coordinates and pointing East direction" in {
  	  val commands = List("PLACE 0,1,North", "MOVE", "RIGHT", "REPORT")
 		  val res = Robot.processCommands(commands).toEither
      val expected = List(PositionResult(Position(Point(0, 1), North)), PointResult(Point(0, 2)), DirectionResult(East), ReportResult("Robot is on the table. Current place: (0,2), direction: East"))
 		  res must beRight[List[Robot.Result]]
 		  res.right.get must beEqualTo(expected)
    }
  }
}