package smofun

import java.io.{ BufferedWriter, File, FileWriter }
import java.util.concurrent.TimeUnit

import breeze.linalg.DenseVector
import smofun.SequentialMinimalOptimization._
import smofun.SmoHelpers.Kernels._
import smofun.SmoHelpers._
import spire.syntax.cfor._

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.{ Random, Try }

object TrainErrorSmoSvmM extends App {

  lazy val parseSvmLightFmt: String => (Vec, Target) =
    line => {
      val bits = line.split(" ")
      val target = bits.head.toInt
      val (fv, _) = bits.slice(1, bits.length)
        .map { b =>
          val sbits = b.split(":")
          val fIndex = sbits.head.toInt
          val fValue = sbits(1).toDouble
          (fIndex, fValue)
        }
        .foldLeft((Seq.empty[Double], 1)) {
          case ((accum, lastIndexSeen), (fIndex, fValue)) =>
            (
              if (fIndex == lastIndexSeen + 1)
                accum :+ fValue

              else {
                val filler = (lastIndexSeen until fIndex).map { _ => 0.0 }
                (accum ++ filler) :+ fValue
              },
              fIndex
            )
        }

    }

  val smoSolver = SequentialMinimalOptimization.train(
    SvmConfig(
      C = 1.0,
      tolerance = 0.001,
      K = gaussian(1.0)
    )
  ) _

  import SequentialMinimalOptimization._

  val data: Seq[(Vec, Target)] =
    Source
      .fromFile(new File("./data/diabetes"))
      .getLines()
      .map { line =>
      }

}