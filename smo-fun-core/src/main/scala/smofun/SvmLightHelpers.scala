package smofun

import java.io.{ BufferedWriter, File, FileWriter }
import breeze.linalg.{ DenseVector, SparseVector }
import scala.io.Source
import scala.language.postfixOps
import scalaz.{ @@, Tag, \/ }

object SvmLightHelpers {

  import SmoHelpers._

  sealed trait Dim
  type Dimensionality = Int @@ Dim

  lazy val whitespaceSplit: String => Seq[String] =
    _.split("\\s+").toSeq

  lazy val separateIndexValue: String => (Int, Double) =
    bit => {
      val sbits = bit.split(":")
      val fIndex = sbits.head.toInt - 1
      val fValue = sbits(1).toDouble
      (fIndex, fValue)
    }

  lazy val svmLightFmtSeparate: String => (Target, Seq[(Int, Double)]) =
    line => {
      val bits = whitespaceSplit(line)
      val target = bits.head.toDouble

      val fv =
        if (bits.length > 1) {
          bits
            .slice(1, bits.length)
            .map { separateIndexValue }

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

  lazy val asSvmLightFmt: SvmDualModel => Seq[String] =
    svm => ???

  lazy val parseOut = (s: String) => s.split("#").head

  lazy val parseSV: Int => String => (DenseVector[Double], Double, Double) =
    dims => line => {

      val bits = whitespaceSplit(line)

      val (target, alpha) = {
        val x = bits.head.toDouble
        (
          if (x < 0.0) -1.0 else 1.0,
          math.abs(x)
        )
      }

      val dv = DenseVector.zeros[Double](dims)
      bits.slice(1, bits.length).foreach { b =>
        val (i, v) = separateIndexValue(b)
        dv(i) = v
      }

      (dv, target, alpha)
    }

  lazy val fromSvmLightFmt: Seq[String] => E[SvmDualModel] =
    lines => \/.fromTryCatchNonFatal {
      // ignore lines(0)
      // ignore lines(1-2) TODO handle more kernels
      val sigma = parseOut(lines(3)).toDouble
      // ignore lines(4-6) TODO handle more kernels
      val dimensionality = parseOut(lines(7)).toInt
      // ignore lines(8)
      val nSvs = parseOut(lines(9)).toInt - 1
      val b = parseOut(lines(10)).toDouble
      // now onto the support vectors!
      val (svs, alphas, targets) =
        lines
          .slice(11, lines.size)
          .map { line => parseSV(dimensionality)(parseOut(line)) }
          .unzip3

      SvmDualModel(
        alphas = alphas.toIndexedSeq,
        targets = targets.toIndexedSeq,
        vectors = svs.toIndexedSeq,
        b = b,
        K = SmoHelpers.Kernels.gaussian(sigma)
      )
    }

  type E[T] = \/[Throwable, T]

  lazy val writeModel: SvmDualModel => File => E[Unit] =
    svm => fi => \/.fromTryCatchNonFatal {
      val w = new BufferedWriter(new FileWriter(fi))
      asSvmLightFmt(svm).foreach { line =>
        w.write(line)
        w.newLine()
      }
      w.close()
    }

  lazy val readModel: File => E[SvmDualModel] =
    fi => \/.fromTryCatchNonFatal {
      Source.fromFile(fi).getLines().toSeq
    }.flatMap { fromSvmLightFmt }

}