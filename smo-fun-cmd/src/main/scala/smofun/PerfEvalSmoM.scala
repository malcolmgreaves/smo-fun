package smofun

import java.io.File

import smofun.SmoHelpers.Kernels._
import smofun.SmoHelpers._

import scala.io.Source
import scala.language.postfixOps
import scala.util.{ Random, Try }

object PerfEvalSmoM extends App {

  import AppHelpers._
  import SvmLightHelpers._

  lazy val smoSolver = SequentialMinimalOptimization.train(conf) _

  /////////////////////////////////////////////////////////////////////////////

  val loc = checksOutAsFile(0, "a labeled data set")(
    Try(new File(args.head))
  ).get
  val doBalanced = Try(args(1).toBoolean).getOrElse(true)
  val trainProp = Try(args(2).toDouble).getOrElse(0.75)
  val doFullAlphaSearch = Try(args(3).toBoolean).getOrElse(true)
  val c = Try(args(4).toDouble).getOrElse(1.0)
  val tol = Try(args(5).toDouble).getOrElse(0.001)
  val gamma = Try(args(6).toDouble).getOrElse(0.5)
  val outModelFi = Try(args(7)).map { x => new File(x) }.getOrElse(new File("./svm_model_out"))
  println(
    s"""Command Line Arguments:
        |Using labeled data from:      $loc
        |Doing +/- balanced training?: $doBalanced
        |Training Proportion:          $trainProp
        |Doing Full Alpha_2 Search?:   $doFullAlphaSearch
        |C (cost parameter):           $c
        |Tolerance for Alpha Change:   $tol
        |Gamma for RBF Kernel:         $gamma
        |Outputting model to:          $outModelFi
     """.stripMargin
  )
  val conf = SvmConfig(
    C = c,
    tolerance = tol,
    K = rbf(gamma),
    doFullAlphaSearch = doFullAlphaSearch
  )

  ////////

  val trainOnly = areEqual(trainProp, 1.0)

  val dimensionality = calculateDimensionality(loc)

  val (train, test) = {
    val parse = parseSvmLightFmt(dimensionality)
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
        println("Not doing any class re-balancing")
        shuffled
      }

    if (trainOnly) {
      println("Not doing any testing, training only!")
      (finalLabeledData, Seq.empty)

    } else
      splitTrainTest(trainProp)(finalLabeledData)
  }

  val svm = {
    println(
      s"""Training on ${train.size} vectors, each of length $dimensionality
          |Using the following SVM training configuration:
          |$conf
          |""".stripMargin
    )
    val (svmModel, trainTime) = time { smoSolver(train) }
    println(
      s"""Finished training in ${trainTime.toSeconds} seconds.
          |Found ${svmModel.size} support vectors.""".stripMargin
    )
    svmModel
  }

  if (!trainOnly) {
    println(s"Now evaluating against ${test.size} examples.")
    val confMat = {
      val marginOf = calcMarginDist(svm)
      val (metrics, testTime) = time {
        test
          .foldLeft(ConfusionMatrix.zero) {
            case (cm, (input, target)) =>
              val pTar = marginOf(input)
              val prediction = pTar > 0.0
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
      val metrics = calcPerf(confMat)
      import metrics._
      import confMat._
      s"""Performance:
          |        +    |   -       <- [Predicted]
          |    -------------------
          | T  |  $tp   |  $fn   |
          |-----------------------
          | F  |  $fp   |  $tn   |
          |    -------------------
          |
          | ^ [Actual]
          |
          |Precision: ${precision * 100.0} %
          |Recall:    ${recall * 100.0} %
          |F1:        ${f1 * 100.0} %
          |Accuracy:  ${accuracy * 100.0} %
          |""".stripMargin
    }
  }

  writeModel(gamma, train.size, svm)(outModelFi).fold(
    e => {
      println(s"Failed to write out model due to: $e")
      throw e
    },
    _ => println("Successfully wrote out model.")
  )
}