import zio.*
import zio.test.*
import zio.stream.*

object Day3Spec extends ZIOSpecDefault {

  val example = (ZStream.fromResource("day3/example.txt")).orDie
  val data    = (ZStream.fromResource("day3/data.txt")).orDie

  def spec = suite("Day3Spec")(
    suite("part1")(
      test("example") {
        for {
          result <- aoc.Day3.part1(example)
        } yield assertTrue(result == 357)
      },
      test("data") {
        for {
          result <- aoc.Day3.part1(data)
        } yield assertTrue(result == 17403)
      }
    ),
    suite("part2")(
      test("example") {
        for {
          result <- aoc.Day3.part2(example)
        } yield assertTrue(result == 3121910778619L)
      },
      test("data") {
        for {
          result <- aoc.Day3.part2(data)
        } yield assertTrue(result == 173416889848394L)
      }
    )
  )
}
