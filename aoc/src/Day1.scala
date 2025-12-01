package aoc

import fastparse.*
import fastparse.Parsed.{Failure, Success}
import zio.*
import zio.stream.*

import NoWhitespace.*

object Day1 {

  enum Direction:
    case L
    case R

  case class Rotation(direction: Direction, steps: Int)

  case class RotationResult(dial: Dial, crossedZeroCount: Int)

  case class Dial private (position: Int):
    def rotateSimple(rotation: Rotation): Dial =
      rotate(rotation).dial

    def rotate(rotation: Rotation): RotationResult =
      val newRawPosition = rotation.direction match
        case Direction.L => (position - rotation.steps)
        case Direction.R => (position + rotation.steps)

      val signChanged  = position != 0 && newRawPosition != 0 && position.sign != newRawPosition.sign
      val landedOnZero = newRawPosition == 0

      val crossingFromWraps         = Math.abs(newRawPosition / 100)
      val crossingFromSignChange    = if signChanged then 1 else 0
      val crossingFromLandingOnZero = if landedOnZero then 1 else 0
      val allCrossings              = crossingFromWraps + crossingFromSignChange + crossingFromLandingOnZero

      val newDial =
        Dial(newRawPosition % 100 match
          case x if x < 0 => x + 100
          case x          => x)

      RotationResult(newDial, allCrossings)

  object Dial:
    def initial = Dial(50)

  object parsing {
    def number[$: P]    = P(CharIn("0-9").rep(1).!.map(_.toInt))
    def direction[$: P] = P(CharIn("LR").!).map({
      case "L" => Direction.L
      case "R" => Direction.R
    })
    def rotation[$: P]  = P(direction ~ number).map { case (dir, num) => Rotation(dir, num) }

    def parseRotation(input: String) = parse(input, c => rotation(using c)) match
      case Success(value, index) => Some(value)
      case _: Failure            => None
  }

  def solutionPart1(input: ZStream[Any, Throwable, Byte]) =
    for {
      res <- input
               .via(ZPipeline.utfDecode)
               .via(ZPipeline.splitLines)
               .map(parsing.parseRotation)
               .some
               .runFold((zeroCount = 0, dial = Dial.initial)) { case ((zeroCount, dial), rotation) =>
                 val newDial      = dial.rotateSimple(rotation)
                 val newZeroCount = if (newDial.position == 0) zeroCount + 1 else zeroCount
                 (newZeroCount, newDial)
               }
    } yield res.zeroCount

  def solutionPart2(input: ZStream[Any, Throwable, Byte]) =
    for {
      res <- input
               .via(ZPipeline.utfDecode)
               .via(ZPipeline.splitLines)
               .map(parsing.parseRotation)
               .some
               .runFold((zeroCount = 0, dial = Dial.initial)) { case ((zeroCount, dial), rotation) =>
                 val rotationResult = dial.rotate(rotation)
                 val newZeroCount   = zeroCount + rotationResult.crossedZeroCount
                 (newZeroCount, rotationResult.dial)
               }
    } yield res.zeroCount

}
