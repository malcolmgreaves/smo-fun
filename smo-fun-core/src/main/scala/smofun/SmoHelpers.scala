package smofun

import java.util.concurrent.TimeUnit

import breeze.linalg.DenseVector
import spire.syntax.cfor._

import scala.concurrent.duration.Duration
import scala.util.Random

object SmoHelpers {

  type Vec = DenseVector[Double]
  type Target = Double

  type Kernel = (Vec, Vec) => Target

  @inline def randomExample(sz: Int, shouldNotBeEqualTo: Int): Int =
    if (sz <= 0)
      throw new IllegalArgumentException(
        "Size must be greater than 0, otherwise we'll loop forever!"
      )

    else if (sz == 1)
      0

    else {
      var i = Random.nextInt(sz)
      while (i == shouldNotBeEqualTo) {
        i = Random.nextInt(sz)
      }
      i
    }

  case class SvmConfig(
    C: Double,
    tolerance: Double,
    K: Kernel
  )

  case class SvmDualModel(
      alphas: IndexedSeq[Double],
      targets: IndexedSeq[Double],
      vectors: IndexedSeq[Vec],
      b: Double,
      K: Kernel
  ) {
    assert(alphas.size == targets.size)
    assert(targets.size == vectors.size)
    val size = alphas.size
  }

  type BinaryClassifier = Vec => Boolean

  lazy val calcMarginDist: SvmDualModel => Vec => Target = {
    case m @ SvmDualModel(alphas, targets, vectors, b, kernel) =>
      val size = m.size
      input => {
        var sum = 0.0
        cfor(0)(_ < size, _ + 1) { i =>
          sum += kernel(vectors(i), input) * targets(i) * alphas(i)
        }
        sum -= b
        ///
        sum
      }
  }

  lazy val onSameSide: (Target, Target) => Boolean =
    (y1, y2) => y1 > 0.0 && y2 > 0.0

  object Initialize {

    def uniform(size: Int)(implicit r: Random): Array[Double] = {
      val x = new Array[Double](size)
      cfor(0)(_ < size, _ + 1) { i =>
        x(i) = r.nextDouble()
      }
      x
    }

    def gaussian(
      mean: Double,
      stdDev: Double
    )(size: Int)(implicit r: Random): Array[Double] = {

      val variance = stdDev * stdDev
      val x = new Array[Double](size)
      cfor(0)(_ < size, _ + 1) { i =>
        x(i) = (r.nextGaussian() + mean) * variance
      }
      x
    }

    def gaussian(size: Int)(implicit r: Random): Array[Double] = {
      val x = new Array[Double](size)
      cfor(0)(_ < size, _ + 1) { i =>
        x(i) = r.nextGaussian()
      }
      x
    }
  }

  object Kernels {

    lazy val linear: Kernel = (v1, v2) => v1.dot(v2)

    type Sigma = Double
    lazy val gaussian: Sigma => Kernel =
      sigma => {
        val denominatorPreCompute = 2.0 * sigma * sigma
        (v1, v2) => {
          val diff = v1 - v2
          math.exp(-diff.dot(diff) / denominatorPreCompute)
        }
      }

  }

  @inline def time[T](x: => T): (T, Duration) = {

    val start = System.nanoTime()
    val result = x
    val end = System.nanoTime()

    (result, Duration(end - start, TimeUnit.NANOSECONDS))
  }

}