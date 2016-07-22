package smofun

import java.io.{ BufferedWriter, File, FileWriter }
import java.util.concurrent.TimeUnit

import breeze.linalg.DenseVector
import smofun.SmoHelpers.Kernels._
import smofun.SmoHelpers._
import spire.syntax.cfor._

import scala.concurrent.duration.Duration
import scala.util.{ Random, Try }

object BenchmarkLooping extends App {

  import BenchmarkHelpers._
  import fif.ImplicitCollectionsData._

  @inline private[this] def simpleMultOp(): Double = {
    5.5 * 2.0 * 44.88 * 122145.0100000100236 * 6636.23151 * 7e7
  }

  val size: Long = Try(args.head.toLong).getOrElse(100000)
  val nTimes = Try(args(1).toInt).getOrElse(10)

  println(
    s"Benchmarking cfor vs while loop on $size iterations, take average over $nTimes"
  )

  /////////////////
  {
    print("JIT warmup...")
    val jitSize = 5000
    // cfor
    cfor(0)(_ < jitSize, _ + 1) { i =>
      val _ = simpleMultOp()
    }
    // while
    var i = 0
    while (i < jitSize) {
      val _ = simpleMultOp()
      i += 1
    }
    // foreach
    (0 until jitSize).foreach { _ =>
      val _ = simpleMultOp()
    }
    println("done")
  }

  /////////////////

  {
    println(s"Running while loop timings...")
    val whileLoopTimings =
      (0 until nTimes).map { _ =>
        val (_, duration) = time {
          var s = 0.0
          var i = 0
          while (i < size) {
            s += simpleMultOp()
            i += 1
          }
          s
        }
        duration
      }

    println(
      timingStatsToStr(
        timingStats[Traversable](whileLoopTimings),
        "while loop"
      )
    )
  }

  /////////////////

  {
    println(s"Running cfor timings...")
    val cforTimings =
      (0 until nTimes).map { _ =>
        val (_, duration) = time {
          var s = 0.0
          cfor(0)(_ < size, _ + 1) { _ =>
            s += simpleMultOp()
          }
          s
        }
        duration
      }

    println(
      timingStatsToStr(
        timingStats[Traversable](cforTimings),
        "cfor"
      )
    )
  }

  /////////////////

  {
    println(s"Running foreach timings...")
    val foreachTimings =
      (0 until nTimes).map { _ =>
        val (_, duration) = time {
          var s = 0.0
          (0l until size).foreach { _ =>
            s += simpleMultOp()
          }
          s
        }
        duration
      }

    println(
      timingStatsToStr(
        timingStats[Traversable](foreachTimings),
        "foreach"
      )
    )
  }

}