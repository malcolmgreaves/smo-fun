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
    C = 1.0,
    tolerance = 0.001,
    K = gaussian(0.5),
    doFullAlphaSearch = false
  )

  val smoSolver = SequentialMinimalOptimization.train(conf) _

  def shuffle[T](xs: Seq[T]): Seq[T] =
    xs
      .map { x => (x, Random.nextInt()) }
      .sortBy { _._2 }
      .map { _._1 }

  lazy val splitPosNeg = (xs: Seq[(Vec, Target)]) =>
    (
      xs.filter { _._2 > 0.0 },
      xs.filter { _._2 < 0.0 }
    )

  lazy val mkBalanced: (Seq[(Vec, Target)], Seq[(Vec, Target)]) => Seq[(Vec, Target)] =
    (pos, neg) =>
      if (pos.size < neg.size)
        pos ++ neg.slice(0, pos.size)
      else
        pos.slice(0, neg.size) ++ neg

  lazy val splitProp = (prop: Double) => (xs: Seq[(Vec, Target)]) => {
    val splitIndx = (xs.size * prop).round.toInt
    (
      xs.slice(0, splitIndx),
      xs.slice(splitIndx, xs.size)
    )
  }

  lazy val splitTrainTest = (prop: Double) => {
    val sp = splitProp(prop)
    (xs: Seq[(Vec, Target)]) => {

      val (pos, neg) = splitPosNeg(xs)
      val (trainPos, testPos) = sp(pos)
      val (trainNeg, testNeg) = sp(neg)
      (
        trainPos ++ trainNeg,
        testPos ++ testNeg
      )
    }
  }

  case class ConfusionMatrix(tp: Int, fp: Int, tn: Int, fn: Int)
  object ConfusionMatrix {
    val zero = ConfusionMatrix(0, 0, 0, 0)
  }

  lazy val calcPerf: ConfusionMatrix => (Double, Double, Double) =
    cm => {
      import cm._

      val (precision, recall) = {
        val tpD = tp.toDouble
        (
          if (tp == 0 && fp == 0) 0.0 else tpD / (tpD + fp),
          if (tp == 0 && tn == 0) 0.0 else tpD / (tpD + tn)
        )
      }
      val f1 =
        if (tp == 0 && (fp == 0 || fn == 0))
          0.0
        else
          (2.0 * precision * recall) / (precision + recall)

      (precision, recall, f1)
    }

  //////////////

  val loc = new File(Try(args.head).getOrElse("./data/diabetes"))
  val doBalanced = Try(args(1).toBoolean).getOrElse(true)
  val trainProp = Try(args(2).toDouble)
    .toOption
    .flatMap { x => if (x > 0.0 && x < 1.0) Some(x) else None }
    .getOrElse(0.75)
  val doLowMemUse = Try(args(3).toBoolean).getOrElse(false)
  println(
    s"""Command Line Arguments:
       |Using labeled data from:      $loc
       |Doing +/- balanced training?: $doBalanced
       |Training Proportion:          $trainProp
       |Predict w/ low memory use?:   $doLowMemUse
     """.stripMargin
  )

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
          |Now evaluating against ${test.size} examples.
     """.stripMargin
    )
    svmModel
  }

  val confMat = {
    val classifier = svmClassifier(doLowMemUse)(svm)
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