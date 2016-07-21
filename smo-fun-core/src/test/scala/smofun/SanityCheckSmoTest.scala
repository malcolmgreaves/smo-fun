package smofun

import java.util.concurrent.TimeUnit

import breeze.linalg.DenseVector
import org.scalatest.FunSuite
import smofun.SequentialMinimalOptimization.SvmConfig

import scala.concurrent.duration._

class SanityCheckSmoTest extends FunSuite {

  import SmoHelpers.Kernels._
  import SanityCheckSmoTest._
  import SequentialMinimalOptimization.predict

  test("Should memorize on tiny, toy data") {

    val (svm, trainingTime) = time {
      SequentialMinimalOptimization.train(
        config = SvmConfig(
          C = 1.0,
          tolerance = 0.0001,
          K = gaussian(1.0)
        ),
        data = dataToy
      )
    }
    val predictOn = predict(svm)

    println(
      s"[Toy] Training using SMO on ${dataToy.size} 2-D examples took ${trainingTime.toMicros} microseconds"
    )

    dataToy foreach {
      case (vec, target) =>
        val predicted = predictOn(vec)
        assert(
          target === predicted,
          s"Expecting target: $target to equal predicted: $predicted"
        )
    }
  }

}

object SanityCheckSmoTest {

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