import zio.*
import zio.test.*
import zio.stream.*

object Day1Spec extends ZIOSpecDefault {

  val example = (ZStream.fromResource("day1/example.txt")).orDie
  val data    = (ZStream.fromResource("day1/data.txt")).orDie

  def spec = suite("Day1Spec")(
    suite("part1")(
      test("example") {
        for {
          result <- aoc.Day1.solutionPart1(example)
        } yield assertTrue(result == 3)
      },
      test("data") {
        for {
          result <- aoc.Day1.solutionPart1(data)
        } yield assertTrue(result == 1147)
      }
    ),
    suite("part2")(
      test("example") {
        for {
          result <- aoc.Day1.solutionPart2(example)
        } yield assertTrue(result == 6)
      },
      test("data") {
        for {
          result <- aoc.Day1.solutionPart2(data)
        } yield assertTrue(result == 6789)
      }
    )
  )
}
