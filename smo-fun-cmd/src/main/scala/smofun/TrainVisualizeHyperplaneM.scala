package smofun

import java.io.File

import breeze.linalg.{ DenseVector, SparseVector }
import smofun.SequentialMinimalOptimization._
import smofun.SmoHelpers.Kernels._
import smofun.SmoHelpers._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try

object TrainVisualizeHyperplaneM extends App {
  import SvmLightHelpers._
  import AppHelpers._

  lazy val smoSolver = SequentialMinimalOptimization.train(conf) _

  /////////////////////////////////////////////////////////////////////////////

  val loc = new File(Try(args.head).getOrElse("./data/diabetes"))
  val doBalanced = Try(args(1).toBoolean).getOrElse(true)
  val trainProp = Try(args(2).toDouble)
    .toOption
    .flatMap { x => if (x > 0.0 && x < 1.0) Some(x) else None }
    .getOrElse(0.75)
  val doLowMemUse = Try(args(3).toBoolean).getOrElse(false)
  val doFullAlphaSearch = Try(args(4).toBoolean).getOrElse(false)
  val c = Try(args(5).toDouble).getOrElse(1.0)
  val tol = Try(args(6).toDouble).getOrElse(0.001)
  val sigma = Try(args(7).toDouble).getOrElse(0.5)
  println(
    s"""Command Line Arguments:
        |Using labeled data from:      $loc
        |Doing +/- balanced training?: $doBalanced
        |Training Proportion:          $trainProp
        |Predict w/ low memory use?:   $doLowMemUse
        |Doing Full Alpha_2 Search?:   $doFullAlphaSearch
        |C (cost parameter):           $c
        |Tolerance for Alpha Change:   $tol
        |S.D. of Gaussian Kernel:      $sigma
     """.stripMargin
  )
  val conf = SvmConfig(
    C = c,
    tolerance = tol,
    K = gaussian(sigma),
    doFullAlphaSearch = doFullAlphaSearch
  )

  ////////

  val dimensionality = calculateDimensionality(loc)
  val parse = parseSvmLightFmt(dimensionality)

  val data = shuffle {
    Source
      .fromFile(loc)
      .getLines()
      .map { parse }
      .toSeq
  }

  println(
    s"""Training on ${data.size} vectors, each of length $dimensionality
        |Using the following SVM training configuration:
        |$conf
     """.stripMargin
  )

  val svm = {
    val (svmModel, trainTime) = time { smoSolver(data) }
    println(
      s"""Finished training in ${trainTime.toSeconds} seconds.
          |Found ${svmModel.size} support vectors.
     """.stripMargin
    )
    svmModel
  }

  /// Visualize Hyperplane

}