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

  import SvmLightHelpers._

  val conf = SvmConfig(
    C = 1.0,
    tolerance = 0.001,
    K = gaussian(1.0),
    doFullAlphaSearch = false
  )

  val smoSolver = SequentialMinimalOptimization.train(conf) _

  //////////////

  val loc = {
    new File(Try(args.head).getOrElse("./data/diabetes"))
  }

  println(s"Using training data from: $loc")

  import SequentialMinimalOptimization._

  val dimensionality = calculateDimensionality(loc)
  val parse = parseSvmLightFmt(dimensionality)

  val data: Seq[(Vec, Target)] =
    Source
      .fromFile(loc)
      .getLines()
      .map { parse }
      .toSeq

  println(
    s"""Training on ${data.size} vectors, each of length $dimensionality
       |Using the following SVM training configuration:
       |$conf
     """.stripMargin
  )

  val (svm, duration) = time { smoSolver(data) }

  println(
    s"Finished training in ${duration.toSeconds} seconds. Found ${svm.size} support vectors."
  )

}