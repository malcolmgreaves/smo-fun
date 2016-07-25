package smofun

import java.io.File
import java.util.concurrent.TimeUnit

import breeze.linalg.{ DenseVector, SparseVector }
import spire.syntax.cfor._

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.language.postfixOps
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

      val fv =
        if (bits.length > 1) {
          bits.slice(1, bits.length)
            .map { b =>
              val sbits = b.split(":")
              val fIndex = sbits.head.toInt - 1
              val fValue = sbits(1).toDouble
              (fIndex, fValue)
            }

        } else
          Seq.empty

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
            if (fvs isEmpty)
              maxSeen

            else {
              val maxInFvs = fvs.map { _._1 }.max
              math.max(maxSeen, maxInFvs)
            }
        } + 1
    }

  lazy val parseSvmLightFmt: Dimensionality => String => (Vec, Target) =
    dimT => {
      val dims = Tag.unwrap(dimT)

      line => {
        val (target, fv) = svmLightFmtSeparate(line)
        val dv =
          if (fv nonEmpty)
            SparseVector(dims)(fv: _*).toDenseVector
          else
            DenseVector.zeros[Double](dims)
        (dv, target)
      }
    }

}