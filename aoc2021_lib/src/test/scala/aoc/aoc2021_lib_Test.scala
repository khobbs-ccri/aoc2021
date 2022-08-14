package aoc

import aoc.aoc2021_lib._
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class aoc2021_lib_Test extends Specification {
  "The AOC 2021 Library" should {
    "solve the 1a example" >> {
      solution_1a(scala.io.Source.fromResource("input_1.txt")) mustEqual 7
    }
    "solve the 1b example" >> {
      solution_1b(scala.io.Source.fromResource("input_1.txt")) mustEqual 5
    }
  }

}
