package smofun

import java.util.concurrent.TimeUnit

import fif.Data

import scala.concurrent.duration.Duration
import scala.language.higherKinds

object BenchmarkHelpers {

  import Data.ops._

  type TimingStats = (Duration, Duration)

  def timingStats[D[_]: Data](timings: D[Duration]): TimingStats = {

    val sumTotalNanos = timings.map { _.toNanos }.reduce { _ + _ }
    val nTest = timings.size

    val averageTime = Duration(sumTotalNanos / nTest, TimeUnit.NANOSECONDS)

    val stddevTime = {
      val meanInNanos: Double = sumTotalNanos.toDouble / nTest.toDouble
      val squaredDeviations =
        timings
          .map { _.toNanos }
          .map { ns =>
            val diff = meanInNanos - ns
            diff * diff
          }
      val avgSqDev =
        squaredDeviations.reduce { _ + _ } / squaredDeviations.size.toDouble
      Duration(math.sqrt(avgSqDev), TimeUnit.NANOSECONDS)
    }

    (averageTime, stddevTime)
  }

  lazy val timingStatsToStr: (TimingStats, String) => String = {
    case ((avg, sd), name) =>
      s"$name took ${avg.toMillis} ms +/- ${sd.toMillis} ( ~${avg.toSeconds} seconds)"
  }

}