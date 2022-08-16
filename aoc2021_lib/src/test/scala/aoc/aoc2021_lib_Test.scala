package aoc

import aoc.aoc2021_lib._
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class aoc2021_lib_Test extends Specification {
  "The AOC 2021 Library" should {
    // 1a
    "solve the 1a example" >> {
      solution_1a(scala.io.Source.fromResource("input_1.txt")) mustEqual 7
    }
    // 1b
    "solve the 1b example" >> {
      solution_1b(scala.io.Source.fromResource("input_1.txt")) mustEqual 5
    }
    // 2a
    "move a sub forward" >> {
      sub_position().interpret_command("forward 15") mustEqual sub_position(15)
    }
    "move a sub down" >> {
      sub_position(). interpret_command("down 6") mustEqual sub_position(0, 6)
    }
    "move a sub up" >> {
      sub_position().interpret_command("up 4") mustEqual sub_position(0, -4)
    }
    "solve the 2a example" >> {
      solution_2a(scala.io.Source.fromResource("input_2.txt")) mustEqual 150
    }
    // 2b
    "move an aimed sub down" >> {
      sub_position().interpret_aimed_command("down 1") mustEqual sub_position(0,0,1)
    }
    "move an aimed sub up" >> {
      sub_position().interpret_aimed_command("up 1") mustEqual sub_position(0,0,-1)
    }
    "move a sub with stored aim forward" >> {
      sub_position(0, 0, 5).interpret_aimed_command("forward 10") mustEqual sub_position(10,50,5)
    }
    "solve the 2b example" >> {
      solution_2b(scala.io.Source.fromResource("input_2.txt")) mustEqual 900
    }
  }

}
