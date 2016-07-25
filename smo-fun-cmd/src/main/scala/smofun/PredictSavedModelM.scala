package smofun

import java.io.{ BufferedWriter, File, FileWriter }

import smofun.SmoHelpers.Kernels._
import smofun.SmoHelpers._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try
import scalaz.Tag

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
  val out = Try(new File(args(2))).getOrElse(new File("svm_prediction_out"))
  val doLowMemUse = Try(args(3).toBoolean).getOrElse(true)
  println(
    s"""Command Line Arguments:
       |Using labeled data from:      $loc
       |Using model from:             $model
       |Outputting predictions to:    $out
       |Low-memory prediction mode?:  $doLowMemUse
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
  val marginOf = calcMarginDist(doLowMemUse)(svm)
  println(s"Loaded SVM model. Now testing...")

  val test: Seq[(Vec, Target)] = {
    val parse = parseSvmLightFmt(Tag[Int, Dim](Tag.unwrap[Int, Dim](svm.dim)))
    Source
      .fromFile(loc)
      .getLines()
      .map { parse }
      .toSeq
  }

  val confMat = {
    val (metrics, testTime) = time {
      test
        .foldLeft(ConfusionMatrix.zero) {
          case (cm, (input, target)) =>
            val pTar = marginOf(input)
            (target > 0.0, pTar > 0.0) match {
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
        |""".stripMargin
  }

  val w = new BufferedWriter(new FileWriter(out))
  try {
    test.foreach {
      case (vec, _) =>
        val predicted = marginOf(vec)
        w.write(s"$predicted")
        w.newLine()
    }
    println(s"Successfully wrote out ${test.size} predictions")

  } catch {
    case e: Exception =>
      throw new IllegalStateException(
        s"Couldn't write out predictions due to: $e",
        e
      )

  } finally { w.close() }

}