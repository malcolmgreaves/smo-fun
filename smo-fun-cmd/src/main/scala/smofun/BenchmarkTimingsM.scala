package smofun

import java.io.{ BufferedWriter, File, FileWriter }
import java.util.concurrent.TimeUnit

import breeze.linalg.DenseVector
import smofun.SequentialMinimalOptimization._
import smofun.SmoHelpers._
import Kernels._
import spire.syntax.cfor._

import scala.concurrent.duration.Duration
import scala.util.{ Random, Try }

object BenchmarkTimingsM extends App {

  val smoSolver = SequentialMinimalOptimization.train(
    SvmConfig(
      C = 1.0,
      tolerance = 0.0001,
      K = gaussian(1.0)
    )
  ) _

  val (nVec, dimensonality, spread, writeOutData, doJitWarmup) = (
    Try(args.head.toInt).toOption.getOrElse(1000),
    Try(args(1).toInt).toOption.getOrElse(100),
    Try(args(2).toInt).toOption.getOrElse(500),
    Try(args(3).toBoolean).toOption.getOrElse(true),
    Try(args(4).toBoolean).toOption.getOrElse(false)
  )

  println(s"Creating benchmark data ($nVec vectors, $dimensonality length each)")

  val (randoData, dataCreationTime) = time {
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
  println(
    s"Done creating benchmark data, took ${dataCreationTime.toSeconds} seconds"
  )

  if (writeOutData) {
    val loc = new File("syntehtic_data_svm-light_fmt")
    val w = new BufferedWriter(new FileWriter(loc))
    randoData.foreach {
      case (x, y) =>
        val bothIdxVal = x.toArray.zipWithIndex.map {
          case (v, i) => s"${i + 1}:$v"
        }
        w.write(s"""$y ${bothIdxVal.mkString(" ")}""")
        w.newLine()
    }
    w.close()
    println(s"Wrote out syntehtic data in svm-light format to: $loc")
  }

  if (doJitWarmup) {
    println("Doing JIT warmup w/ 1 training pass...")
    // crappy JIT warmup sequence ...
    val (_, jitWarmupTime) = time { val _ = smoSolver(randoData) }
    println(
      s"benchmark: JIT warmup sequence complete, took ${jitWarmupTime.toSeconds} seconds"
    )

  } else {
    println("No JIT warmup, staight to training & timing")
  }

  // do timing tests
  val nTest = 3
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
    s"Took an average of ${asDurationAvg.toMillis} +/- ${stdDev.toMillis} ms (~ ${asDurationAvg.toSeconds} seconds) for $nTest runs on $nVec examples of $dimensonality length each"
  )

}