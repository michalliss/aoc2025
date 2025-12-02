package aoc

import fastparse.*
import fastparse.Parsed.{Failure, Success}
import zio.*
import zio.stream.*

import NoWhitespace.*

object Day2 {

  extension (x: Int) {
    def **(exp: Int): Int = {
      Math.pow(x, exp).toInt
    }
  }

  extension (x: Long) {
    def digitCount: Int = {
      if x == 0 then 1
      else Math.log10(Math.abs(x)).toInt + 1
    }

    def **(exp: Long): Long = {
      Math.pow(x, exp).toLong
    }

    // splits the number into two halves, left and right
    // if odd number of digits, right half gets the extra digit
    def split: (Long, Long) = {
      val divisor = 10 ** ((digitCount + 1) / 2)
      val left    = x / divisor
      val right   = x % divisor
      (left, right)
    }

  }

  case class InvalidId(value: Long) {}

  object InvalidId {
    def fromFirstHalf(part: Long): InvalidId = {
      InvalidId((part * (10 ** part.digitCount) + part))
    }
  }

  case class InvalidId2(value: Long) {}

  object InvalidId2 {
    def fromRepeated(part: Long, count: Int): InvalidId2 = {
      val partDigits = part.digitCount
      val multiplier = 10 ** partDigits
      val result     = (0 until count).foldLeft(0L) { (acc, _) =>
        acc * multiplier + part
      }
      InvalidId2(result)
    }
  }

  case class IdRange(min: Long, max: Long) {
    def invalid: List[InvalidId] = {
      val (l, _) = min.split
      LazyList
        .iterate(l)(_ + 1)
        .map(InvalidId.fromFirstHalf)
        .dropWhile(_.value < min)
        .takeWhile(_.value <= max)
        .toList
    }

    def invalid2: List[InvalidId2] = {
      val digitNumbersToCheck = Range.inclusive(1, max.digitCount / 2)

      digitNumbersToCheck
        .map { dn =>
          val valuesToCheck = Range.inclusive(10 ** (dn - 1), 10 ** dn - 1)
          valuesToCheck.flatMap { value =>
            LazyList
              .iterate(2)(_ + 1)
              .map(count => InvalidId2.fromRepeated(value, count))
              .dropWhile(_.value < min)
              .takeWhile(_.value <= max)
          }
        }
        .flatten
        .toList
        .distinct
    }
  }

  object parsing {
    def number[$: P]              = P(CharIn("0-9").rep(1).!.map(s => s.toLong))
    def range[$: P]               = P(number ~ "-" ~ number).map { case (min, max) => IdRange(min, max) }
    def parseRange(input: String) = parse(input, c => range(using c)) match
      case Success(value, index) => Some(value)
      case _: Failure            => None
  }

  def part1(input: ZStream[Any, Throwable, Byte]) =
    for {
      sum <- input
               .via(ZPipeline.utf8Decode)
               .via(ZPipeline.splitOn(","))
               .map(line => parsing.parseRange(line))
               .some
               .map(_.invalid)
               .flattenIterables
               .map(_.value)
               .runSum
    } yield sum

  def part2(input: ZStream[Any, Throwable, Byte]) =
    for {
      sum <- input
               .via(ZPipeline.utf8Decode)
               .via(ZPipeline.splitOn(","))
               .map(line => parsing.parseRange(line))
               .some
               .map(_.invalid2)
               .flattenIterables
               .map(_.value)
               .runSum
    } yield sum

}
