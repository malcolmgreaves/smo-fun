package smofun

import java.awt
import java.io.File

import breeze.linalg.{ DenseVector, SparseVector }
import com.quantifind.charts.highcharts.{ Highchart, Series, SeriesType }
import com.quantifind.charts.repl.IterablePair
import smofun.SequentialMinimalOptimization._
import smofun.SmoHelpers.Kernels._
import smofun.SmoHelpers._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try

object TrainVisualizeHyperplaneM extends App {
  import SvmLightHelpers._
  import AppHelpers._

  /////////////////////////////////////////////////////////////////////////////

  import SvmLightHelpers._
  import AppHelpers._

  /////////////////////////////////////////////////////////////////////////////

  val loc = Try(new File(args(1))).getOrElse(new File("data/SupportVectorMachineWithGaussianKernel_svmlight"))
  val doBalanced = false
  val doFullAlphaSearch = true
  val c = 1.0
  val tol = 0.001
  val gamma = Try(args.head.toDouble).getOrElse(0.01)
  println(
    s"""Configuration
        |Using labeled data from:      $loc
        |Doing +/- balanced training?: $doBalanced
        |Doing Full Alpha_2 Search?:   $doFullAlphaSearch
        |C (cost parameter):           $c
        |Tolerance for Alpha Change:   $tol
        |Gamma for RBF Kernel:         $gamma
     """.stripMargin
  )
  val smoSolver = SequentialMinimalOptimization.train(
    SvmConfig(
      C = c,
      tolerance = tol,
      K = rbf(gamma),
      doFullAlphaSearch = doFullAlphaSearch
    )
  ) _

  ////////

  val dimensionality = calculateDimensionality(loc)

  val data = {
    val parse = parseSvmLightFmt(dimensionality)
    val origData: Seq[(Vec, Target)] =
      Source
        .fromFile(loc)
        .getLines()
        .map { parse }
        .toSeq
    val shuffled = shuffle(origData)
    val (pos, neg) = splitPosNeg(shuffled)

    println(s"${pos.size} + examples and ${neg.size} - examples")

    val finalLabeledData =
      if (doBalanced) {
        val balanced = mkBalanced(pos, neg)
        println(
          s"""Doing + and - class balancing
              |Balanced size: ${balanced.size}""".stripMargin
        )
        balanced

      } else {
        println("Not doing any class rebalancing")
        shuffled
      }

    finalLabeledData
  }

  val svm = {
    println(s"Training on ${data.size} vectors, each of length $dimensionality")
    val (svmModel, trainTime) = time { smoSolver(data) }
    println(
      s"""Finished training in ${trainTime.toSeconds} seconds.
          |Found ${svmModel.size} support vectors.
     """.stripMargin
    )
    svmModel
  }

  /// Visualize Hyperplane

  val classifier = svmClassifier(svm)

  val (colorPredPos, colorPredNeg) = (awt.Color.RED, awt.Color.BLUE)
  val (colorActPos, colorActNeg) = (awt.Color.ORANGE, awt.Color.GREEN)

  val xs = data.map { case (vec, _) => vec(0) }
  val ys = data.map { case (vec, _) => vec(1) }

  val everything = data.map {
    case (vec, target) =>
      (classifier(vec), target > 0.0, vec(0), vec(1))
  }

  val (predictedPos, predictedNeg) = {
    val byPredict = everything.groupBy { _._1 }
    (
      byPredict.getOrElse(true, Seq.empty),
      byPredict.getOrElse(false, Seq.empty)
    )
  }

  val (actualPos, actualNeg) = {
    val byActual = everything.groupBy { _._2 }
    (
      byActual.getOrElse(true, Seq.empty),
      byActual.getOrElse(false, Seq.empty)
    )
  }

  import com.quantifind.charts.Highcharts._
  import com.quantifind.charts.highcharts

  lazy val splitXY: Seq[(_, _, Double, Double)] => (Seq[Double], Seq[Double]) =
    values => (values.map { _._3 }, values.map { _._4 })

  def better_scatter(
    x: Seq[Double],
    y: Seq[Double],
    color: awt.Color
  ): Highchart = {

    import Highchart._
    plot(
      Highchart(
        Series(
          data = x.zip(y).toSeq,
          chart = Some(SeriesType.scatter),
          color = Some(highcharts.Color.javaColorToHex(color))
        )
      )
    )
  }

  val sp1 = {
    val (x, y) = splitXY(predictedPos)
    better_scatter(x, y, colorPredPos)
  }

  hold()

  val sp2 = {
    val (x, y) = splitXY(predictedNeg)
    better_scatter(x, y, colorPredNeg)
  }

  title("Predicted")
  yAxis("Feature Value #1")
  xAxis("Feature Value #2")

  legend(Seq("RED is predicted +", "BLUE is predicted -"))

  unhold()

  val sp3 = {
    val (x, y) = splitXY(actualPos)
    better_scatter(x, y, colorActPos)
  }

  hold()

  val sp4 = {
    val (x, y) = splitXY(actualNeg)
    better_scatter(x, y, colorActNeg)
  }

  title("Actual")
  yAxis("Feature Value #1")
  xAxis("Feature Value #2")
  legend(Seq("ORANGE is actual +", "GREEN is actual -"))

}
