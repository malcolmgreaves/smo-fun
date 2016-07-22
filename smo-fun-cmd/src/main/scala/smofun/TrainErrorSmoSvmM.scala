package smofun

import java.io.File

import breeze.linalg.DenseVector
import smofun.SequentialMinimalOptimization._
import smofun.SmoHelpers.Kernels._
import smofun.SmoHelpers._

import scala.io.Source

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

      (DenseVector(fv.toArray), target)
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
      .map { parseSvmLightFmt }
      .toSeq

  val dimensionality = {

    val labels = data.map { _._2 }.toSet
    if (labels.size != 2)
      throw new IllegalStateException(
        s"Expecting binary labeled data, actually have ${labels.size} labels!!\n\n$labels\n"
      )

    val vecSizes = data.map { _._1.length }.toSet
    if (vecSizes.size == 1)
      vecSizes.head
    else
      throw new IllegalStateException(
        s"Each vector must have the same size, found ${vecSizes.size} difference size!!\n\n$vecSizes\n"
      )
  }

  println(
    s"Training on ${data.size} vectors, each of length $dimensionality"
  )

  val (svm, duration) = time { smoSolver(data) }

  println(
    s"Finished training in ${duration.toSeconds} seconds"
  )

}