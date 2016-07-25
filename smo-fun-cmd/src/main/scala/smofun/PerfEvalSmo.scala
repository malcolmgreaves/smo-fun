package smofun

import java.io.File

import smofun.SmoHelpers.Kernels._
import smofun.SmoHelpers._

import scala.io.Source
import scala.language.postfixOps
import scala.util.{ Random, Try }

object PerfEvalSmo extends App {

  import SvmLightHelpers._

  val conf = SvmConfig(
    C = 1.25,
    tolerance = 0.001,
    K = gaussian(0.05),
    doFullAlphaSearch = false
  )

  val smoSolver = SequentialMinimalOptimization.train(conf) _

  //////////////

  val loc = {
    new File(Try(args.head).getOrElse("./data/diabetes"))
  }

  println(s"Using training data from: $loc")

  val dimensionality = calculateDimensionality(loc)
  val parse = parseSvmLightFmt(dimensionality)

  val data: Seq[(Vec, Target)] =
    Source
      .fromFile(loc)
      .getLines()
      .map { parse }
      .toSeq

  val (train, test) = {

    val shuffled =
      data
        .map { x => (x, Random.nextInt()) }
        .sortBy { _._2 }
        .map { _._1 }

    val pos = shuffled.filter { _._2 > 0.0 }
    val neg = shuffled.filter { _._2 < 0.0 }

    val balanced =
      if (pos.size < neg.size)
        pos ++ neg.slice(0, pos.size)
      else
        pos.slice(0, neg.size) ++ neg

    println(
      s"""${pos.size} + examples and ${neg.size} - examples
         |Balanced size: ${balanced.size}
       """.stripMargin
    )

    (balanced, balanced)

    //    val splitIndx = {
    //      val trainProp = 0.75
    //      println(s"Using ${trainProp * 100.0} % for training, rest for test")
    //      (shuffled.size * trainProp).round.toInt
    //    }
    //
    //    (
    //      shuffled.slice(0, splitIndx),
    //      shuffled.slice(splitIndx, shuffled.size)
    //    )
  }

  println(
    s"""Training on ${train.size} vectors, each of length $dimensionality
       |Using the following SVM training configuration:
       |$conf
     """.stripMargin
  )

  val svm = {
    val (svmModel, trainTime) = time { smoSolver(train) }

    println(
      s"""Finished training in ${trainTime.toSeconds} seconds.
          |Found ${svmModel.size} support vectors.
          |Now evaluating against ${test.size} examples.
     """.stripMargin
    )
    svmModel
  }

  case class ConfusionMatrix(tp: Int, fp: Int, tn: Int, fn: Int)
  object ConfusionMatrix {
    val zero = ConfusionMatrix(0, 0, 0, 0)
  }

  val confMat = {
    val classifier = svmClassifier(svm)
    val (metrics, testTime) = time {
      test
        .foldLeft(ConfusionMatrix.zero) {
          case (cm, (input, target)) =>
            val prediction = classifier(input)
            val targetIsTrue = target > 0.0
            (targetIsTrue, prediction) match {
              case (true, true) => cm.copy(tp = cm.tp + 1)
              case (true, false) => cm.copy(fn = cm.fn + 1)
              case (false, true) => cm.copy(fp = cm.fp + 1)
              case (false, false) => cm.copy(tn = cm.tn + 1)
            }
        }
    }
    println(s"Finished testing in ${testTime.toSeconds} seconds.")
    metrics
  }

  lazy val calcPerf: ConfusionMatrix => (Double, Double, Double) =
    cm => {
      import cm._
      val (precision, recall) = {
        val tpD = tp.toDouble
        (
          tpD / (tpD + fp),
          tpD / (tpD + tn)
        )
      }
      val f1 =
        (2.0 * precision * recall) / (precision + recall)

      (precision, recall, f1)
    }

  println {
    val (precision, recall, f1) = calcPerf(confMat)
    import confMat._
    s"""Performance:
       |        +    |   -
       |     ---------------
       | T  |  $tp   | $tn  |
       |---------------------
       | F  |  $fp   | $fn  |
       |    ----------------
       |
       |Precision: ${precision * 100.0} %
       |Recall:    ${recall * 100.0} %
       |F1:        ${f1 * 100.0} %
     """.stripMargin
  }

}