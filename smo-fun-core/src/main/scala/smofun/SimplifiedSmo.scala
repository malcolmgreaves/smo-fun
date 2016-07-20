package smofun

import breeze.linalg.DenseVector

import spire.syntax.cfor._

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

    def examineExample(example: Vec, label: Target): Int = {
      ???
    }

    var numChanged = 0
    var examineAll = true

    while(numChanged > 0 || examineAll) {

      numChanged = 0

      //
      // update alphas
      //
      if(examineAll) {
        data.foreach {
          case (example, label) =>
            numChanged += examineExample(example, label)
        }

      } else {

        val nonBoundExamples =  alphas
          .zipWithIndex
          .filter {
            case (a, index) =>
              val alphaIsZero = !(a > 0) && !(a < 0.0)
              val alphaIsC = a == C
              !alphaIsZero && !alphaIsC
          }
          .map { case (_, index) => index }


        nonBoundExamples.foreach { i =>
          val (example, label) = data(i)
          numChanged += examineExample(example, label)
        }
      }
      //
      // done updating alphas
      //

      if(examineAll)
        examineAll = false

      else if(numChanged == 0)
        examineAll = true
    }


    alphas.toSeq
  }

  def predict(data: Seq[Vec], as: Alphas)(input: Vec): Double = ???

}