package aoc

import aoc.aoc2021_lib._

import scala.io.Source

/**
 * @author ${user.name}
 */
object aoc2021_app {

  def main(args : Array[String]): Unit = {
    val problem_name = args(0)
    val file = Source.fromFile(args(1))
    val result = problem_name match {
      case "1a" => solution_1a(file)
      case "1b" => solution_1b(file)
      case "2a" => solution_2a(file)
      case "2b" => solution_2b(file)
      case "3a" => solution_3a(file)
    }
    println(result)
  }
}
