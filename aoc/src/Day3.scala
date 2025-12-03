package aoc

import fastparse.*
import fastparse.Parsed.{Failure, Success}
import zio.*
import zio.stream.*

import NoWhitespace.*
import scala.annotation.tailrec

object Day3 {
  type Joltage = Long
  case class Battery(joltage: Joltage)
  type Bank    = List[Battery]
  case class Index(value: Int)

  extension (bank: Bank)
    def maxIndexLeavingSpaceOnRight(space: Int): Index =
      Index(bank.zipWithIndex.dropRight(space).maxBy(_._1.joltage)._2)

    def maxJoltage(batteriesCount: Int): Joltage = {
      @tailrec
      def helper(result: List[Joltage], remaining: Bank): List[Joltage] =
        if result.size == batteriesCount then result
        else {
          val spaceNeeded = batteriesCount - result.size - 1
          val index       = remaining.maxIndexLeavingSpaceOnRight(spaceNeeded).value
          helper(result :+ remaining(index).joltage, remaining.drop(index + 1))
        }

      helper(Nil, bank).mkString.toLong
    }

  object parsing {
    def digit[$: P] = P(CharIn("0-9").!).map(_.toLong).map(Battery.apply)
    def bank[$: P]  = digit.rep.map(_.toList)

    def parseBank(input: String) = parse(input, c => bank(using c)) match
      case Success(value, index) => Some(value)
      case _: Failure            => None
  }

  def part1(input: ZStream[Any, Throwable, Byte]) =
    for {
      res <- input
               .via(ZPipeline.utf8Decode)
               .via(ZPipeline.splitLines)
               .map(line => parsing.parseBank(line))
               .some
               .map(_.maxJoltage(2))
               .runSum
    } yield res

  def part2(input: ZStream[Any, Throwable, Byte]) =
    for {
      res <- input
               .via(ZPipeline.utf8Decode)
               .via(ZPipeline.splitLines)
               .map(line => parsing.parseBank(line))
               .some
               .map(_.maxJoltage(12))
               .runSum
    } yield res
}
