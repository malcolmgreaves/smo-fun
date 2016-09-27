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
  object Dimensionality {
    def apply(x: Int): Dimensionality = Tag[Int,Dim](x)
    def apply(x: Dimensionality): Int = Tag.unwrap(x)
  }

  type E[T] = \/[Throwable, T]

  lazy val separateIndexValue: String => (Int, Double) =
    bit => {
      val sbits = bit.split(":")
      val fIndex = sbits.head.toInt - 1
      val fValue = sbits(1).toDouble
      (fIndex, fValue)
    }

  lazy val svmLightFmtSeparate: String => (Target, Seq[(Int, Double)]) =
    line => {
      val bits = line.split("\\s+")
      val target = bits.head.toDouble

      val fv =
        if (bits.length > 1) {
          bits
            .slice(1, bits.length)
            .map { separateIndexValue }
            .toSeq

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

  lazy val writeVectorSvmLight: Vec => String =
    dv => dv
      .activeKeysIterator
      .filter { i => math.abs(dv(i)) > 1e-20 }
      .map { i => s"${i + 1}:${dv(i)}" }
      .mkString(" ")

  type ModelOutInfo = (Double, Int, SvmDualModel)

  lazy val asSvmLightFmt: ModelOutInfo => Seq[String] = {
    case (gamma, nTrain, svm) =>
      val header = Seq(
        //        "Trained using smo-fun (https://github.com/malcolmgreaves/smo-fun)",
        "SVM-light Version V6.02",
        "2 # kernel type",
        "3 # kernel parameter -d",
        s"$gamma # kernel parameter -g",
        "1 # kernel parameter -s",
        "1 # kernel parameter -r",
        "empty# kernel parameter -u",
        s"${svm.supportVectors.head.data.length} # highest feature index, svm-light does 1-based indexing :*(",
        s"$nTrain # number of training documents",
        s"${svm.size + 1} # number of support vectors plus 1",
        s"${svm.b} # threshold b, each following line is a SV (starting with alpha*y then the features in the index:value space-delim. format)"
      )

      val modelSVs = svm.supportVectors.zip(svm.bothAlphaTargets)
        .map {
          case (sv, bothAlphaTarget) =>
            s"$bothAlphaTarget ${writeVectorSvmLight(sv)} #"
        }

      header ++ modelSVs
  }

  lazy val parseOut: String => String =
    _.split("#").head.trim

  lazy val parseSV: Dimensionality => String => (DenseVector[Double], Double) =
    dims => {
      val parse = parseSvmLightFmt(dims)
      line => {
        val (dv, bothAlphaTarget) = parse(line)
        (dv, bothAlphaTarget)
      }
    }

  lazy val fromSvmLightFmt: Seq[String] => E[SvmDualModel] =
    lines => \/.fromTryCatchNonFatal {
      // ingore lines(0) -- unnecessary "version" tring
      val kernelTypeInt = lines(1).toInt
      lazy val polyKernelPow = lines(2).toDouble
      lazy val gamma = parseOut(lines(3)).toDouble
      lazy val sEitherSigmoidPoly = lines(4).toDouble
      lazy val cEitherSigmoidPoly = lines(5).toDouble
      // ignore lines(6) -- "custom" kernel parameter
      val dimensionality = Dimensionality(parseOut(lines(7)).toInt)
      // ignore lines(8)

      val kernelF: Kernel = kernelTypeInt match {
        case 0 =>
          SmoHelpers.Kernels.linear

        case 1 =>
          // use:
          // polyKernelPow
          // sEitherSigmoidPoly
          // cEitherSigmoidPoly
          throw new IllegalArgumentException("Cannot handle polynomial kernel!")

        case 2 =>
          SmoHelpers.Kernels.rbf(gamma)

        case 3 =>
          // use:
          // sEitherSigmoidPoly
          // cEitherSigmoidPoly
          throw new IllegalArgumentException("Cannot handle sigmoid tanh kernel!")

        case unk =>
          throw new IllegalArgumentException(
            s"Unrecognized & unable to handle kernel intgeter code: $unk"
          )
      }

      val nSvs = parseOut(lines(9)).toInt - 1
      val b = parseOut(lines(10)).toDouble
      // now onto the support vectors!
      val (svs, bothATs) =
        lines
          .slice(11, lines.size)
          .map { line => parseSV(dimensionality)(parseOut(line)) }
          .unzip

      SvmDualModel(
        bothAlphaTargets = bothATs.toIndexedSeq,
        supportVectors = svs.toIndexedSeq,
        b = b,
        K = kernelF
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