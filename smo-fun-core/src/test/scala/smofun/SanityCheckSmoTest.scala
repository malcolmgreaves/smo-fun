package smofun

import java.util.concurrent.TimeUnit

import breeze.linalg.DenseVector
import org.scalatest.FunSuite
import smofun.SequentialMinimalOptimization.SvmConfig
import smofun.SmoHelpers.Kernels._
import spire.syntax.cfor._

import scala.concurrent.duration._
import scala.util.Random

class SanityCheckSmoTest extends FunSuite {

  import SanityCheckSmoTest._
  import SequentialMinimalOptimization.{ calcMarginDist, onSameSide }

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

  test("Synthetic benchmark, 10k vectors, 1k dimensions each") {
    import SequentialMinimalOptimization.{ Vec, Target }

    val (nVec, dimensonality, spread) = (10000, 1000, 500)

    val randoData: Seq[(Vec, Target)] =
      (0 until nVec).map { _ =>
        val x = DenseVector(
          SmoHelpers.Initialize.uniform(dimensonality)(Random.self)
        )
        cfor(0)(_ < x.length, _ + 1) { i =>
          x(i) *= Random.nextInt(spread).toDouble
        }
        val y = if (Random.nextBoolean()) 1d else -1d
        (x, y)
      }

    // crappy JIT warmup sequence ...
    cfor(0)(_ < 5, _ + 1) { _ =>
      val _ = smoSolver(randoData)
    }
    println(s"benchmark: JIT warmup sequence complete")

    // do timing tests
    val nTest = 5
    val trainingTimes =
      (0 until nTest)
        .map { _ =>
          val (_, trainingTime) = time { smoSolver(randoData) }
          trainingTime
        }

    val sumTotalNanos = trainingTimes.map { _.toNanos }.sum

    val asDurationAvg = Duration(sumTotalNanos / nTest, TimeUnit.NANOSECONDS)

    val stdDev = {
      val meanInNanos: Double = sumTotalNanos.toDouble / nTest.toDouble
      val squaredDeviations =
        trainingTimes
          .map { _.toNanos }
          .map { ns =>
            val diff = meanInNanos - ns
            diff * diff
          }
      val avgSqDev = squaredDeviations.sum / squaredDeviations.size.toDouble

      Duration(math.sqrt(avgSqDev), TimeUnit.NANOSECONDS)
    }

    println(
      s"Took an average of ${asDurationAvg.toSeconds} +/- ${stdDev.toSeconds} seconds for $nTest runs on $nVec examples of $dimensonality length each"
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

  @inline def time[T](x: => T): (T, Duration) = {

    val start = System.nanoTime()
    val result = x
    val end = System.nanoTime()

    (result, Duration(end - start, TimeUnit.NANOSECONDS))
  }

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