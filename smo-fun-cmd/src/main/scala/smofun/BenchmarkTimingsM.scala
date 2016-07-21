package smofun

import java.util.concurrent.TimeUnit

import breeze.linalg.DenseVector
import smofun.SequentialMinimalOptimization._
import smofun.SmoHelpers._
import Kernels._
import spire.syntax.cfor._

import scala.concurrent.duration.Duration
import scala.util.Random

object BenchmarkTimingsM extends App {

  val smoSolver = SequentialMinimalOptimization.train(
    SvmConfig(
      C = 1.0,
      tolerance = 0.0001,
      K = gaussian(1.0)
    )
  ) _

  val (nVec, dimensonality, spread) = (100, 20, 500)
  println(s"Creating benchmark data ($nVec vectors, $dimensonality length each)")

  val (randoData, dataCreationTime)= time {
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
  }
  println(s"Done creating benchmark data, took ${dataCreationTime.toSeconds} seconds")

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