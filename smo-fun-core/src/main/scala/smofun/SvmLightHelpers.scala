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

  type E[T] = \/[Throwable, T]

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

  lazy val writeVectorSvmLight: DenseVector[Double] => String =
    dv => dv
      .activeKeysIterator
      .map { i => s"${i + 1}:${dv(i)}" }
      .mkString(" ")

  type ModelOutInfo = (Double, Int, SvmDualModel)

  lazy val asSvmLightFmt: ModelOutInfo => Seq[String] = {
    case (sigma, nTrain, svm) =>
      val header = Seq(
        //        "Trained using smo-fun (https://github.com/malcolmgreaves/smo-fun)",
        "SVM-light Version V6.02",
        "2 # kernel type",
        "3 # kernel parameter -d",
        s"$sigma # kernel parameter -g",
        "1 # kernel parameter -s",
        "1 # kernel parameter -r",
        "empty# kernel parameter -u",
        s"${svm.vectors.head.data.length} # highest feature index",
        s"$nTrain # number of training documents",
        s"${svm.size + 1} # number of support vectors plus 1",
        s"${svm.b} # threshold b, each following line is a SV (starting with alpha*y)"
      )

      val modelSVs = svm.vectors.zip(svm.alphas).zip(svm.targets)
        .map {
          case ((sv, alpha), target) =>
            s"${alpha * target} ${writeVectorSvmLight(sv)} #"
        }

      header ++ modelSVs
  }

  lazy val parseOut: String => String =
    _.split("#").head.trim

  lazy val parseSV: Dimensionality => String => (DenseVector[Double], Double, Double) =
    dims => {
      val parse = parseSvmLightFmt(dims)
      line => {
        val (dv, bothAlphaTarget) = parse(line)
        val (target, alpha) = {
          val x = bothAlphaTarget
          (
            if (x < 0.0) -1.0 else 1.0,
            math.abs(x)
          )
        }
        (dv, target, alpha)
      }
    }

  lazy val fromSvmLightFmt: Seq[String] => E[SvmDualModel] =
    lines => \/.fromTryCatchNonFatal {
      // ignore lines(0)
      // ignore lines(1-2) TODO handle more kernels
      val sigma = parseOut(lines(3)).toDouble
      // ignore lines(4-6) TODO handle more kernels
      val dimensionality = Tag[Int, Dim](parseOut(lines(7)).toInt)
      // ignore lines(8)
      val nSvs = parseOut(lines(9)).toInt - 1
      val b = parseOut(lines(10)).toDouble
      // now onto the support vectors!
      val (svs, targets, alphas) =
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

  lazy val writeModel: ModelOutInfo => File => E[Unit] =
    mo => fi => \/.fromTryCatchNonFatal {
      val w = new BufferedWriter(new FileWriter(fi))
      asSvmLightFmt(mo).foreach { line =>
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