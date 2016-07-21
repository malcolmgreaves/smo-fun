package smofun

import breeze.linalg.DenseVector
import spire.syntax.cfor._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

object SequentialMinimalOptimization {

  import SmoHelpers._

  type Vec = DenseVector[Double]
  type Target = Double

  type Kernel = (Vec, Vec) => Target

  @inline def randomExample(sz: Int, notEqualTo: Int): Int = {
    if (sz <= 1)
      throw new IllegalArgumentException("Size must be greater than 1, otherwise we'll loop forever!")

    var i = Random.nextInt(sz)
    while (i != notEqualTo) {
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

  type Classifier = Vec => Target

  lazy val predict: SvmDualModel => Classifier = {
    case m @ SvmDualModel(alphas, targets, vectors, b, kernel) =>
      val size = m.size
      val combinedTargetAlphas = {
        val preCompTargetAlphs = new Array[Double](size)
        cfor(0)(_ < size, _ + 1) { i =>
          preCompTargetAlphs(i) = targets(i) * alphas(i)
        }
        preCompTargetAlphs.toIndexedSeq
      }

      input => {
        var sum = 0.0
        cfor(0)(_ < size, _ + 1) { i =>
          sum += combinedTargetAlphas(i) * kernel(vectors(i), input)
        }
        sum -= b
        ///
        sum
      }
  }

  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  def train(
    config: SvmConfig,
    data: Seq[(Vec, Target)]
  ): SvmDualModel = {

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

    var b = 0.0

    @inline def predict(index: Int): Target = {
      val input = vecOnly(index)
      var sum = 0.0
      cfor(0)(_ < size, _ + 1) { i =>
        // TODO [mg] need a check here? does it make sense to predict against itself?
        if (i != index) {
          sum += targetOnly(i) * alphas(i) * K(vecOnly(i), input)
        }
      }
      sum -= b
      ///
      sum
    }

    val errorCache = new Array[Double](size)

    def takeStep(i1: Int, i2: Int): Boolean =
      if (i1 == i2) {
        false

      } else {

        val alph1 = alphas(i1)
        val y1 = targetOnly(i1)
        val e1 = predict(i1) - y1

        val alph2 = alphas(i2)
        val y2 = targetOnly(i2)
        val e2 = predict(i2) - y2

        val s = y1 * y2

        val (l, h) =
          if (y1 == y2)
            (
              math.max(0, alph2 + alph1 - C),
              math.min(C, alph2 + alph1)
            )
          else
            (
              math.max(0.0, alph2 - alph1),
              math.min(C, C + alph2 - alph1)
            )

        if (l == h) {
          false

        } else {

          val (k11, k12, k22) = {
            val (v1, v2) = (vecOnly(i1), vecOnly(i2))
            (K(v1, v1), K(v1, v2), K(v2, v2))
          }

          val eta = k11 + k22 - 2.0 * k12
          val newAlpha2 =
            if (eta > 0.0) {

              val a2 = alph2 + y2 * ((e1 - e2) / eta)
              if (a2 < l)
                l
              else if (a2 > h)
                h
              else
                a2

            } else {

              val (lObjFnEval, hObjFnEval) = {

                val f1 = y1 * (e1 + b) - alph1 * k11 - s * alph2 * k12
                val f2 = y2 * (e2 + b) - s * alph1 * k12 - alph2 * k22

                val l1 = alph1 + s * (alph2 - l)
                val h1 = alph1 + s * (alph2 - h)

                val phiL =
                  l1 * f1 + l * f2 + 0.5 * (l1 * l1) * k11 + 0.5 * (l * l) * k22 + s * l * l1 * k12

                val phiH =
                  h1 * f1 + h * f2 + 0.5 * (h1 * h1) * k11 + 0.5 * (h * h) * k22 + s * h * h1 * k12

                (phiL, phiH)
              }

              if (lObjFnEval < hObjFnEval - tolerance)
                l
              else if (lObjFnEval > hObjFnEval + tolerance)
                h
              else
                alph2
            }

          if (math.abs(newAlpha2 - alph2) < tolerance * (newAlpha2 + alph2 + tolerance))
            false

          else {

            val newAlpha1 = alph1 + s * (alph2 - newAlpha2)

            // store the new, calculated lagrange multipliers
            alphas(i1) = newAlpha1
            alphas(i2) = newAlpha2

            // update our error cache with the SVM predictions on i1,i2 using
            // the updated alphas
            errorCache(i1) = predict(i1) - y1
            errorCache(i2) = predict(i2) - y2

            // calculate the threshold update
            b =
              if (0.0 < newAlpha1 && newAlpha1 < C) {
                val b1 = b - e1 - y1 * (newAlpha1 - alph1) * k11 - y2 * (newAlpha2 - alph2) * k12
                b1

              } else if (0.0 < newAlpha2 && newAlpha2 < C) {
                val b2 = b - e2 - y1 * (newAlpha1 - alph1) * k12 - y2 * (newAlpha2 - alph2) * k22
                b2

              } else {
                val b1 = b - e1 - y1 * (newAlpha1 - alph1) * k11 - y2 * (newAlpha2 - alph2) * k12
                val b2 = b - e2 - y1 * (newAlpha1 - alph1) * k12 - y2 * (newAlpha2 - alph2) * k22
                0.5 * (b1 + b2)
              }

            // yes we changed alphas!
            true
          }
        }
      }

    def examineExample(index: Int): Int = {

      val y2 = targetOnly(index)
      val alph2 = alphas(index)
      val e2 = predict(index) - y2
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

    // MAIN LOOP --> heuristic for selecting alpha_1

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

    // finished training, output model information

    SvmDualModel(
      alphas = alphas.toIndexedSeq,
      targets = targetOnly.toIndexedSeq,
      vectors = vecOnly.toIndexedSeq,
      b = b,
      K = K
    )
  }

}