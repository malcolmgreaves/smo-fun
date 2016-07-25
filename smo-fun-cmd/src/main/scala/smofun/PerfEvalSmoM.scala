package smofun

import java.io.File

import smofun.SmoHelpers.Kernels._
import smofun.SmoHelpers._

import scala.io.Source
import scala.language.postfixOps
import scala.util.{ Random, Try }

object PerfEvalSmoM extends App {

  import SvmLightHelpers._
  import AppHelpers._

  lazy val smoSolver = SequentialMinimalOptimization.train(conf) _

  /////////////////////////////////////////////////////////////////////////////

  val loc = new File("data/SupportVectorMachineWithGaussianKernel_svmlight")
  val doBalanced = false
  val trainProp = 0.75
  val doLowMemUse = true
  val doFullAlphaSearch = true
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

  val (train, test) = {
    val data: Seq[(Vec, Target)] =
      Source
        .fromFile(loc)
        .getLines()
        .map { parse }
        .toSeq
    val shuffled = shuffle(data)
    val (pos, neg) = splitPosNeg(shuffled)

    println(
      s"""${pos.size} + examples and ${neg.size} - examples
         |Using ${trainProp * 100.0} % for training, rest for test.
       """.stripMargin
    )

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

    splitTrainTest(trainProp)(finalLabeledData)
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
          |# NaNs from training: ${svmModel.alphas.count { _.isNaN }}
          |Now evaluating against ${test.size} examples.
     """.stripMargin
    )
    svmModel
  }

  val confMat = {
    val marginOf = calcMarginDist(doLowMemUse)(svm)
    val classifier = svmClassifier(doLowMemUse)(svm)
    val (metrics, testTime) = time {
      test
        .foldLeft(ConfusionMatrix.zero) {
          case (cm, (input, target)) =>
            val pTar = marginOf(input)
            val prediction = pTar > 0.0
            //            val prediction = classifier(input)
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