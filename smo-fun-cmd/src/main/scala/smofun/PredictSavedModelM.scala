package smofun

import java.io.{ BufferedWriter, File, FileWriter, IOException }

import smofun.SmoHelpers._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try

object PredictSavedModelM extends App {

  import AppHelpers._
  import SvmLightHelpers._

  /////////////////////////////////////////////////////////////////////////////

  val loc = checksOutAsFile(0, "a labeled data set")(
    Try(new File(args.head))
  ).get
  val model = checksOutAsFile(1, "an svm-light formatted model")(
    Try(new File(args(1)))
  ).get
  val out = Try(new File(args(2))).getOrElse(new File("svm_predictions_out"))
  println(
    s"""Command Line Arguments:
       |Using labeled data from:      $loc
       |Using model from:             $model
       |Outputting predictions to:    $out
     """.stripMargin
  )

  ////////

  val svm = readModel(model).fold(
    e => throw new IllegalStateException(
      s"Couldn't load model due to $e",
      e
    ),
    identity
  )
  val marginOf = calcMarginDist(svm)
  println(
    s"""Loaded SVM model:
       |# Support Vectors: ${svm.size}
       |Dimensionality:    ${svm.dim}
       |Bias:              ${svm.b}
       |
       |Now testing...""".stripMargin
  )

  val test: Seq[(Vec, Target)] = {
    val parse = parseSvmLightFmt(svm.dim)
    Source
      .fromFile(loc)
      .getLines()
      .map { parse }
      .toSeq
  }

  val w = new BufferedWriter(new FileWriter(out))
  var nWrote = 0
  try {

    val confMat = {
      val (metrics, testTime) = time {
        test
          .foldLeft(ConfusionMatrix.zero) {
            case (cm, (input, target)) =>

              val predicted = {
                val distanceToHyperplane = marginOf(input)
                try {
                  w.write(s"$distanceToHyperplane")
                  w.newLine()
                  nWrote += 1
                } catch {
                  case e: IOException =>
                    println(
                      s"[ERROR][skipping] Could not write prediction $distanceToHyperplane due to $e"
                    )
                }
                distanceToHyperplane > 0.0
              }
              val actual = target > 0.0

              (predicted, actual) match {
                case (true, true) => cm.addTruePositive()
                case (true, false) => cm.addFalsePositive()
                case (false, true) => cm.addFalseNegative()
                case (false, false) => cm.addTrueNegative()
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

    if (nWrote == test.size)
      println(s"Successfully wrote out $nWrote predictions")
    else
      println(
        s"Partial success: Successfully wrote out $nWrote of ${test.size} predictions, failed to write out the others"
      )

  } finally { w.close() }

}