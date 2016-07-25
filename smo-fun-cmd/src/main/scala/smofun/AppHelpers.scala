package smofun

import java.io.File

import smofun.SmoHelpers._

import scala.language.postfixOps
import scala.util.{ Random, Try }

object AppHelpers {

  def shuffle[T](xs: Seq[T]): Seq[T] =
    xs
      .map { x => (x, Random.nextInt()) }
      .sortBy { _._2 }
      .map { _._1 }

  lazy val splitPosNeg = (xs: Seq[(Vec, Target)]) =>
    (
      xs.filter { _._2 > 0.0 },
      xs.filter { _._2 < 0.0 }
    )

  lazy val mkBalanced: (Seq[(Vec, Target)], Seq[(Vec, Target)]) => Seq[(Vec, Target)] =
    (pos, neg) =>
      if (pos.size < neg.size)
        pos ++ neg.slice(0, pos.size)
      else
        pos.slice(0, neg.size) ++ neg

  lazy val splitProp = (prop: Double) => (xs: Seq[(Vec, Target)]) => {
    val splitIndx = (xs.size * prop).round.toInt
    (
      xs.slice(0, splitIndx),
      xs.slice(splitIndx, xs.size)
    )
  }

  lazy val splitTrainTest = (prop: Double) => {
    val sp = splitProp(prop)
    (xs: Seq[(Vec, Target)]) => {

      val (pos, neg) = splitPosNeg(xs)
      val (trainPos, testPos) = sp(pos)
      val (trainNeg, testNeg) = sp(neg)
      (
        trainPos ++ trainNeg,
        testPos ++ testNeg
      )
    }
  }

  case class ConfusionMatrix(tp: Int, fp: Int, tn: Int, fn: Int)
  object ConfusionMatrix {
    val zero = ConfusionMatrix(0, 0, 0, 0)
  }

  lazy val calcPerf: ConfusionMatrix => (Double, Double, Double) =
    cm => {
      import cm._

      val (precision, recall) = {
        val tpD = tp.toDouble
        (
          if (tp == 0 && fp == 0) 0.0 else tpD / (tpD + fp),
          if (tp == 0 && tn == 0) 0.0 else tpD / (tpD + tn)
        )
      }
      val f1 =
        if (tp == 0 && (fp == 0 || fn == 0))
          0.0
        else
          (2.0 * precision * recall) / (precision + recall)

      (precision, recall, f1)
    }

  lazy val checksOutAsFile: (Int, String) => Try[File] => Try[File] =
    (argIdx, message) => _
      .recover {
        case _ => throw new IllegalArgumentException(
          s"Must supply $message as the argument $argIdx!"
        )
      }
      .map { x =>
        if (!x.isFile)
          throw new IllegalArgumentException(
            s"Argument $argIdx ($message) must be a file! This doesn't input suffice: $x"
          )
        else
          x
      }

}