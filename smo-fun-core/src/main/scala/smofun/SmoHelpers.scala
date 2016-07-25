package smofun

import java.util.concurrent.TimeUnit

import breeze.linalg.{ DenseVector, SparseVector }
import smofun.SvmLightHelpers.{ Dim, Dimensionality }
import spire.syntax.cfor._

import scala.concurrent.duration.Duration
import scala.language.postfixOps
import scala.util.Random
import scalaz.Tag

object SmoHelpers {

  type Vec = DenseVector[Double]
  type Target = Double

  type Kernel = (Vec, Vec) => Target

  type CacheKernelEval = (Int, Int) => Target

  val defaultDoubleEqualityTol = 1e-8

  @inline def areEqual(
    a: Target,
    b: Target,
    tolerance: Double = defaultDoubleEqualityTol
  ): Boolean =
    math.abs(a - b) < tolerance

  @inline def isZero(
    y: Target,
    tolerance: Double = defaultDoubleEqualityTol
  ): Boolean =
    math.abs(y) < tolerance

  //  lazy val createKernelCache: Seq[(Vec, Target)] => CacheKernelEval = ???

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
    K: Kernel,
    doFullAlphaSearch: Boolean = false
  )

  case class SvmDualModel(
      bothAlphaTarget: IndexedSeq[Double],
      supportVectors: IndexedSeq[Vec],
      b: Double,
      K: Kernel
  ) {
    assert(bothAlphaTarget.size == supportVectors.size)
    assert(supportVectors nonEmpty)

    val size = supportVectors.size
    val dim: Dimensionality = Tag[Int, Dim](supportVectors.head.data.length)
  }

  type BinaryClassifier = Vec => Boolean

  lazy val svmClassifier: SvmDualModel => BinaryClassifier =
    svm => {
      val marginOf = calcMarginDist(svm)
      input => marginOf(input) > 0.0
    }

  lazy val calcMarginDist: SvmDualModel => Vec => Target = {
    case m @ SvmDualModel(bothAlphaTarget, supportVectors, b, kernel) =>
      val size = m.size
      input => {
        var sum = 0.0
        cfor(0)(_ < size, _ + 1) { i =>
          sum += kernel(supportVectors(i), input) * bothAlphaTarget(i)
        }
        sum -= b
        ///
        sum
      }
  }

  lazy val onSameSide: (Target, Target) => Boolean =
    (y1, y2) => (y1 > 0.0 && y2 > 0.0) || (y1 < 0.0 && y2 < 0.0)

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
        val denominatorPreCompute = -2.0 * sigma * sigma
        (v1, v2) => {
          val diff = v1 - v2
          math.exp(
            math.sqrt(diff.dot(diff)) / denominatorPreCompute
          )
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