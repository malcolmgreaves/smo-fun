package smofun

import java.io.File
import java.util.concurrent.TimeUnit

import breeze.linalg.{ DenseVector, SparseVector }
import spire.syntax.cfor._

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Random
import scalaz.{ @@, Tag }

object SvmLightHelpers {

  import SmoHelpers._

  sealed trait Dim
  type Dimensionality = Int @@ Dim

  lazy val whitespaceSplit: String => Seq[String] =
    _.split("\\s+").toSeq

  lazy val svmLightFmtSeparate: String => (Target, Seq[(Int, Double)]) =
    line => {
      val bits = whitespaceSplit(line)
      val target = bits.head.toDouble
      val fv = bits.slice(1, bits.length)
        .map { b =>
          val sbits = b.split(":")
          val fIndex = sbits.head.toInt - 1
          val fValue = sbits(1).toDouble
          (fIndex, fValue)
        }
      (target, fv)
    }

  lazy val calculateDimensionality: File => Dimensionality =
    fi => Tag[Int, Dim] {
      Source
        .fromFile(fi)
        .getLines()
        .foldLeft(0) {
          case (maxSeen, line) =>
            val (_, fvs) = svmLightFmtSeparate(line)
            val maxInFvs = fvs.map { _._1 }.max
            math.max(maxSeen, maxInFvs)
        }
    }

  lazy val parseSvmLightFmt: Dimensionality => String => (Vec, Target) =
    dimT => {
      val dims = Tag.unwrap(dimT)

      line => {
        val bits = whitespaceSplit(line)
        val target = bits.head.toDouble
        val fv = bits.slice(1, bits.length)
          .map { b =>
            val sbits = b.split(":")
            val fIndex = sbits.head.toInt - 1
            val fValue = sbits(1).toDouble
            (fIndex, fValue)
          }

        (
          SparseVector(dims)(fv: _*).toDenseVector,
          target
        )
      }
    }

}