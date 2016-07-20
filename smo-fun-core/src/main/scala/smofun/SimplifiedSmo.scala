package smofun

import breeze.linalg.DenseVector
import spire.syntax.cfor._

import scala.collection.mutable
import scala.util.Random

object SimplifiedSmo {

  import SmoHelpers._

  type Vec = DenseVector[Double]
  type Target = Double
  type Alphas = Seq[Double]

  case class SvmConfig(
    C: Double
  )

  def train(config: SvmConfig, data: Seq[(Vec, Target)]): Alphas = {

    import config._

    val vecOnly = data.map { _._1 }
    val alphas = Initialize.uniform(data.size)(Random.self)

    def examineExample(index: Int): Int = {
      ???

      val alph2 = alphas(index)

    }

    var numChanged = 0
    var examineAll = true

    while (numChanged > 0 || examineAll) {

      numChanged = 0

      //
      // update alphas
      //
      if (examineAll) {
        cfor(0)(_ < data.size, _ + 1) { i =>
          numChanged += examineExample(i)
        }

      } else {

        val nonBoundExamples = {

          val x = mutable.ArrayBuilder.make[Int]()
          x.sizeHint(data.size / 2)

          cfor(0)(_ < data.size, _ + 1) { i =>
            val a = alphas(i)
            val alphaIsZero = !(a > 0) && !(a < 0.0)
            val alphaIsC = a == C

            if (!alphaIsZero && !alphaIsC) {
              val _ = x += i
            }
          }

          x.result()
        }

        cfor(0)(_ < nonBoundExamples.length, _ + 1) { i =>
          numChanged += examineExample(i)
        }
      }
      //
      // done updating alphas
      //

      if (examineAll)
        examineAll = false

      else if (numChanged == 0)
        examineAll = true
    }

    alphas.toSeq
  }

  def predict(data: Seq[Vec], as: Alphas)(input: Vec): Double = ???

}