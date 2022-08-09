package aoc

import scala.io.Source

/**
 * @author ${user.name}
 */
object App {

  // 1a
  def check_depth_increases(
                             prior_depth: Option[Int],
                             depth_list: List[Int]
                           ): Int = {
    (prior_depth,depth_list) match {
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

  def main(args : Array[String]): Unit = {
    val filename = args(0)
    val file = Source.fromFile(filename)
    val depths = file.getLines().map(_.toInt).toList
    val increase_count = check_depth_increases_windowed(None, depths)
    println(increase_count)
  }
}
