package aoc

import scala.io.BufferedSource
import scala.util.matching.Regex


object aoc2021_lib {

  // 1a
  def solution_1a( file: BufferedSource ): Int = {
    val depths = file.getLines().map(_.toInt).toList
    check_depth_increases(None, depths)
  }
  def check_depth_increases(
                             prior_depth: Option[Int],
                             depth_list: List[Int]
                           ): Int = {
    (prior_depth, depth_list) match {
      case (_, Nil) => 0
      case (None, (depth: Int) :: tail) =>
        check_depth_increases(Some(depth), tail)
      case (Some(prior_depth), (depth: Int) :: tail) =>
        if (prior_depth < depth)
          1 + check_depth_increases(Some(depth), tail)
        else
          check_depth_increases(Some(depth), tail)
    }
  }

  // 1b
  def solution_1b(file: BufferedSource): Int = {
    val depths = file.getLines().map(_.toInt).toList
    check_depth_increases_windowed(None, depths)
  }
  def check_depth_increases_windowed(
                                      prior_sum: Option[Int],
                                      depth_list: List[Int]
                                    ): Int = {
    val window = depth_list.take(3)
    if (window.size < 3) return 0
    val window_sum = window.sum
    prior_sum match {
      case None =>
        check_depth_increases_windowed(Some(window_sum), depth_list.tail)
      case Some(prior_sum) =>
        if (prior_sum < window_sum)
          1 + check_depth_increases_windowed(Some(window_sum), depth_list.tail)
        else
          check_depth_increases_windowed(Some(window_sum), depth_list.tail)
    }
  }

  // 2a
  case class sub_position( horizontal: Int = 0, depth: Int = 0, aim: Int = 0 ) {
    val command: Regex = raw"(forward|down|up) (\d+)".r
    def interpret_command(input: String): sub_position = {
      input match {
        case command( "forward", f ) => sub_position(horizontal + f.toInt, depth )
        case command( "down"   , d ) => sub_position(horizontal          , depth + d.toInt )
        case command( "up"     , u ) => sub_position(horizontal          , depth - u.toInt )
      }
    }

    def interpret_aimed_command(input: String): sub_position = {
      input match {
        case command("forward", f) =>
          val forward = f.toInt
          sub_position(horizontal + forward, depth + aim * forward, aim )
        case command("down",    d) => sub_position(horizontal, depth, aim + d.toInt)
        case command("up",      u) => sub_position(horizontal, depth, aim  - u.toInt)
      }
    }

    def position_product: Int = horizontal * depth
  }
  def solution_2a(file: BufferedSource): Int = {
    file
      .getLines()
      .foldLeft(sub_position())(
        (sp: sub_position, c:String) => sp.interpret_command(c)
      ).position_product
  }

  def solution_2b(file: BufferedSource): Int = {
    file
      .getLines()
      .foldLeft(sub_position())(
        (sp: sub_position, c: String) => sp.interpret_aimed_command(c)
      ).position_product
  }
}
