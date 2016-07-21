package smofun

import java.util.concurrent.TimeUnit

import spire.syntax.cfor._

import scala.concurrent.duration.Duration
import scala.util.Random

object SmoHelpers {

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

    import SequentialMinimalOptimization.Kernel

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