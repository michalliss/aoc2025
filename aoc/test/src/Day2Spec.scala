import zio.*
import zio.test.*
import zio.stream.*
import aoc.Day2

object Day2Spec extends ZIOSpecDefault {

  import aoc.Day2.*

  val example = (ZStream.fromResource("day2/example.txt")).orDie
  val data    = (ZStream.fromResource("day2/data.txt")).orDie

  def spec = suite("Day2Spec")(
    suite("unit tests")(
      suite("id splitting")(
        test("single digit") {
          val (left, right) = 5.split
          assertTrue(left == 0 && right == 5)
        },
        test("even number of digits") {
          val (left, right) = 12.split
          assertTrue(left == 1 && right == 2)
        },
        test("odd number of digits") {
          val (left, right) = 123.split
          assertTrue(left == 1 && right == 23)
        }
      ),
      suite("InvalidId.fromFirstHalf")(
        test("single digit") {
          val id = InvalidId.fromFirstHalf(5)
          assertTrue(id.value == 55)
        },
        test("two digits") {
          val id = InvalidId.fromFirstHalf(12)
          assertTrue(id.value == 1212)
        },
        test("three digits") {
          val id = InvalidId.fromFirstHalf(123)
          assertTrue(id.value == 123123)
        }
      ),
      suite("InvalidId2.fromPart")(
        test("single digit, repeat 3") {
          val id = InvalidId2.fromRepeated(5, 3)
          assertTrue(id.value == 555)
        },
        test("two digits, repeat 2") {
          val id = InvalidId2.fromRepeated(12, 2)
          assertTrue(id.value == 1212)
        },
        test("two digits, repeat 4") {
          val id = InvalidId2.fromRepeated(12, 4)
          assertTrue(id.value == 12121212)
        }
      ),
      suite("invalid id generation")(
        test("1-10") {
          val invalidId = IdRange(1, 10).invalid
          assertTrue(invalidId.size == 0)
        },
        test("10-100") {
          val ids = IdRange(10, 100).invalid
          assertTrue(ids == List(11, 22, 33, 44, 55, 66, 77, 88, 99).map(InvalidId(_)))
        },
        test("10-100") {
          val ids = IdRange(123, 1010).invalid
          assertTrue(ids == List(1010).map(InvalidId(_)))
        }
      ),
      suite("invalid id2 generation")(
        test("1-10") {
          val invalidId = IdRange(1, 10).invalid2
          assertTrue(invalidId.size == 0)
        },
        test("10-100") {
          val ids = IdRange(10, 100).invalid2
          assertTrue(ids == List(11, 22, 33, 44, 55, 66, 77, 88, 99).map(InvalidId2(_)))
        },
        test("10-100") {
          val ids = IdRange(123, 1112).invalid2
          assertTrue(ids == List(1111, 222, 333, 444, 555, 666, 777, 888, 999, 1010).map(InvalidId2(_)))
        }
      )
    ),
    suite("part 1")(
      test("example") {
        for {
          result <- Day2.part1(example)
        } yield assertTrue(result == 1227775554L)
      },
      test("data") {
        for {
          result <- Day2.part1(data)
        } yield assertTrue(result == 23039913998L)
      }
    ),
    suite("part 2")(
      test("example") {
        for {
          result <- Day2.part2(example)
        } yield assertTrue(result == 4174379265L)
      },
      test("data") {
        for {
          result <- Day2.part2(data)
        } yield assertTrue(result == 35950619148L)
      }
    )
  )
}
