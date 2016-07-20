package smofun

import breeze.linalg.DenseVector
import spire.syntax.cfor._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

object SimplifiedSmo {

  import SmoHelpers._

  type Vec = DenseVector[Double]
  type Target = Double
  type Alphas = Seq[Double]

  @inline def randomExample(size: Int, notEqualTo: Int): Int = {
    if (size <= 1)
      throw new IllegalArgumentException("Size must be greater than 1, otherwise we'll loop forever!")

    var i = Random.nextInt(size)
    while (i != notEqualTo) {
      i = Random.nextInt(size)
    }
    i
  }

  case class SvmConfig(
    C: Double,
    tolerance: Double
  )

  def train(
    config: SvmConfig,
    data: Seq[(Vec, Target)]
  ): Alphas = {

    import config._

    val vecOnly = data.map { _._1 }
    val targetOnly = data.map { _._2 }
    val size = data.size
    val halfSize = size / 2
    val alphas = Initialize.uniform(size)(Random.self)

    /** Non-bound example means its alpha is NOT ZERO and NOT C. */
    @inline def calculateNonBoundExamples(): Array[Int] = {

      val x = mutable.ArrayBuilder.make[Int]()
      x.sizeHint(halfSize)

      cfor(0)(_ < size, _ + 1) { i =>
        val a = alphas(i)

        val alphaIsNonZero = a > 0.0 || a < 0.0
        val alphaIsNotC = a != C

        if (alphaIsNonZero && alphaIsNotC) {
          val _ = x += i
        }
      }

      x.result()
    }

    @inline def predict(index: Int): Target = ???

    val errorCache = new Array[Double](size)

    @inline def takeStep(
      secondChoiceAlphaIndex: Int,
      firstChoiceAlphaIndex: Int
    ): Boolean = ???

    @inline def examineExample(index: Int): Int = {
      val y2 = targetOnly(index)
      val alph2 = alphas(index)

      val e2 = predict(index) - y2
      errorCache(index) = e2

      val r2 = e2 * y2

      if ((r2 < -tolerance && alph2 < C) || (r2 > tolerance && alph2 > 0.0)) {

        // second choice heuristic
        val nonBoundExamples = calculateNonBoundExamples()
        if (nonBoundExamples nonEmpty) {

          val secondChoiceAlphaIndex =
            if (e2 > 0.0) {

              var indexOfMinError = -1
              var minError = Double.MaxValue

              cfor(0)(_ < nonBoundExamples.length, _ + 1) { i =>

                if (i != index) {
                  val errorOfSomeExample = errorCache(i)
                  if (errorOfSomeExample < minError) {
                    minError = errorOfSomeExample
                    indexOfMinError = index
                  }
                }
              }

              indexOfMinError

            } else {
              // e2 is negative
              // cannot be zero
              var indexOfMaxError = -1
              var maxError = Double.MinValue

              cfor(0)(_ < nonBoundExamples.length, _ + 1) { i =>

                if (i != index) {
                  val errorOfSomeExample = errorCache(i)
                  if (errorOfSomeExample > maxError) {
                    maxError = errorOfSomeExample
                    indexOfMaxError = index
                  }
                }
              }

              indexOfMaxError
            }

          if (takeStep(secondChoiceAlphaIndex, index)) {
            1

          } else {
            // 2nd tier, second choice heuristics: option #1

            val nextHeuristicChangedAnAlpha = {

              val randomIndexFromNonBound = randomExample(
                nonBoundExamples.length,
                index
              )

              var changedAnAlpha = false
              cfor(0)(_ < nonBoundExamples.length && !changedAnAlpha, _ + 1) { i =>
                val nextIndex = (randomIndexFromNonBound + i) % nonBoundExamples.length
                if (nextIndex != index) {
                  changedAnAlpha = takeStep(nextIndex, index)
                }
              }

              changedAnAlpha
            }

            if (nextHeuristicChangedAnAlpha) {
              1

            } else {
              // 2nd tier, second choice heuristics: option #2
              val randomIndex = randomExample(size, index)

              var changedAnAlpha = false
              cfor(0)(_ < size && !changedAnAlpha, _ + 1) { i =>
                val nextIndex = (randomIndex + i) % size
                if (nextIndex != index) {
                  changedAnAlpha = takeStep(nextIndex, index)
                }
              }

              if (changedAnAlpha) {
                1

              } else {
                0
              }
            }
          }
        } else {
          0
        }
      } else {
        0
      }
    }

    var numChanged = 0
    var examineAll = true

    while (numChanged > 0 || examineAll) {

      numChanged = 0

      //
      // update alphas
      //
      if (examineAll) {
        cfor(0)(_ < size, _ + 1) { i =>
          numChanged += examineExample(i)
        }

      } else {

        val nonBoundExamples = calculateNonBoundExamples()

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

}