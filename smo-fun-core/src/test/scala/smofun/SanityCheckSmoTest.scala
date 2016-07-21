package smofun


import breeze.linalg.DenseVector
import org.scalatest.FunSuite
import smofun.SequentialMinimalOptimization._
import smofun.SmoHelpers._
import Kernels._



class SanityCheckSmoTest extends FunSuite {
  import SanityCheckSmoTest._

  test("Should memorize on tiny, toy data") {

    val (svm, trainingTime) = time { smoSolver(dataToy) }
    val predict = calcMarginDist(svm)

    println(
      s"[Toy] Training using SMO on ${dataToy.size} examples (2D each) took ${trainingTime.toMillis} ms"
    )

    val incorrectResults =
      dataToy
        .map {
          case (vec, target) =>
            val predicted = predict(vec)
            (
              onSameSide(target, predicted),
              s"Expecting target: $target to equal predicted: $predicted"
            )
        }
        .filter { case (isCorrect, _) => !isCorrect }

    val allCorrect = incorrectResults.isEmpty
    assert(
      !allCorrect,
      s"""Failed on ${incorrectResults.size} out of ${dataToy.size} examples:
         |${"\t"}${incorrectResults.map { _._2 }.mkString("\n\t")}
       """.stripMargin
    )
  }

}

object SanityCheckSmoTest {

  lazy val smoSolver = SequentialMinimalOptimization.train(
    SvmConfig(
      C = 1.0,
      tolerance = 0.0001,
      K = gaussian(1.0)
    )
  ) _

  lazy val inputsToy = Seq(
    DenseVector(-7d, -4d),
    DenseVector(-9d, -8d),
    DenseVector(2d, 5d),
    DenseVector(-3d, -10d),
    DenseVector(9d, 7d),
    DenseVector(3d, 8d),
    DenseVector(8d, 11d),
    DenseVector(8d, 9d)
  )

  lazy val targetsToy = Seq(
    -1d,
    -1d,
    -1d,
    -1d,
    1d,
    1d,
    1d,
    1d
  )

  import SequentialMinimalOptimization.{ Vec, Target }
  lazy val dataToy: Seq[(Vec, Target)] = inputsToy.zip(targetsToy)

  assert(inputsToy.size == targetsToy.size)
}