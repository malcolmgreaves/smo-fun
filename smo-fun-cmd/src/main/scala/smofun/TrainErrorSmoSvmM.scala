package smofun

import java.io.File

import breeze.linalg.{ DenseVector, SparseVector }
import smofun.SequentialMinimalOptimization._
import smofun.SmoHelpers.Kernels._
import smofun.SmoHelpers._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try

object TrainErrorSmoSvmM extends App {

  lazy val whitespaceSplit: String => Seq[String] =
    _.split("\\s+").toSeq

  lazy val parseSvmLightFmt: String => (Seq[(Int, Double)], Target) =
    line => {
      val bits = whitespaceSplit(line)
      val target = bits.head.toDouble
      //      val (fv, _) = bits.slice(1, bits.length)
      //        .map { b =>
      //          val sbits = b.split(":")
      //          val fIndex = sbits.head.toInt
      //          val fValue = sbits(1).toDouble
      //          (fIndex, fValue)
      //        }
      //        .foldLeft((Seq.empty[Double], 1)) {
      //          case ((accum, lastIndexSeen), (fIndex, fValue)) =>
      //            (
      //              if (fIndex == lastIndexSeen + 1)
      //                accum :+ fValue
      //
      //              else {
      //                val filler = (lastIndexSeen until fIndex).map { _ => 0.0 }
      //                (accum ++ filler) :+ fValue
      //              },
      //              fIndex
      //            )
      //        }
      val fv = bits.slice(1, bits.length)
        .map { b =>
          val sbits = b.split(":")
          val fIndex = sbits.head.toInt - 1
          val fValue = sbits(1).toDouble
          (fIndex, fValue)
        }

      (fv, target)
    }

  val smoSolver = SequentialMinimalOptimization.train(
    SvmConfig(
      C = 1.0,
      tolerance = 0.001,
      K = gaussian(1.0)
    )
  ) _

  //////////////

  val loc = {
    new File(Try(args.head).getOrElse("./data/diabetes"))
  }

  println(s"Using training data from: $loc")

  import SequentialMinimalOptimization._

  val rawSparseData =
    Source
      .fromFile(loc)
      .getLines()
      .map { parseSvmLightFmt }
      .toSeq

  val dimensionality = {

    val labels = rawSparseData.map { _._2 }.toSet
    if (labels.size != 2)
      throw new IllegalStateException(
        s"Expecting binary labeled data, actually have ${labels.size} labels!!\n\n$labels\n"
      )

    val maxFeatIndexPerVec =
      rawSparseData
        .map {
          case (fv, _) =>
            val fIndicies = fv.map { case (fIndex, _) => fIndex }
            if (fIndicies isEmpty)
              0
            else
              fIndicies.max
        }
    if (maxFeatIndexPerVec isEmpty)
      0
    else
      maxFeatIndexPerVec.max + 1
  }

  val data: Seq[(Vec, Target)] =
    rawSparseData
      .map {
        case (sv, target) =>
          (
            SparseVector(dimensionality)(sv: _*).toDenseVector,
            target
          )
      }

  println(
    s"Training on ${rawSparseData.size} vectors, each of length $dimensionality"
  )

  val (svm, duration) = time { smoSolver(data) }

  println(
    s"Finished training in ${duration.toSeconds} seconds. Found ${svm.size} support vectors."
  )

}