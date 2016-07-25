package smofun

import breeze.linalg.{ DenseMatrix, DenseVector }
import spire.syntax.cfor._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

object SequentialMinimalOptimization {

  import SmoHelpers._

  def train(config: SvmConfig)(data: Seq[(Vec, Target)]): SvmDualModel = {

    import config._

    val size = data.size
    val alphas = DenseVector[Double](Initialize.uniform(size, C)(Random.self))

    // Non-bound example means its alpha is NOT ZERO and NOT C.
    @inline def calculateNonBoundExamples(): Array[Int] = {

      val x = new mutable.ArrayBuffer[Int]()

      cfor(0)(_ < size, _ + 1) { i =>
        val a = alphas(i)

        if (!isZero(a) && !areEqual(a, C)) {
          x.append(i)
        }
      }
      x.toArray[Int]
    }

    val vecOnly = data.map { _._1 }.toIndexedSeq
    val targetOnly = DenseVector[Double](data.map { _._2 }.toArray)
    var b = 0.0

    val kernelEvalCache: DenseMatrix[Target] = {

      val m = DenseMatrix.zeros[Double](size, size)

      cfor(0)(_ < size, _ + 1) { i =>
        cfor(0)(_ < size, _ + 1) { j =>
          m(i, j) = K(vecOnly(i), vecOnly(j))
        }
      }

      m
    }

    @inline def predict(index: Int): Target = {
      val sum: Double = kernelEvalCache(::, index) dot (targetOnly :* alphas)
      sum - b
    }

    val errorCache: DenseVector[Double] = {
      // initialize the error cache
      val e = DenseVector.zeros[Double](size)
      cfor(0)(_ < size, _ + 1) { i =>
        e(i) = predict(i) - targetOnly(i)
      }
      e
    }

    def takeStep(input1: Int, input2: Int): Boolean =
      if (input1 == input2) {
        false

      } else {

        val (i1, i2) = (input2, input1)
        //        val (i1, i2) = (input1, input2)

        val alph1 = alphas(i1)
        val y1 = targetOnly(i1)
        val e1 = errorCache(i1)

        val alph2 = alphas(i2)
        val y2 = targetOnly(i2)
        val e2 = errorCache(i2)

        val s = y1 * y2

        val (l, h) =
          if (areEqual(y1, y2))
            (
              math.max(0, alph2 + alph1 - C),
              math.min(C, alph2 + alph1)
            )
          else
            (
              math.max(0.0, alph2 - alph1),
              math.min(C, C + alph2 - alph1)
            )

        if (areEqual(l, h)) {
          false

        } else {

          val (k11, k12, k22) = {
            (
              kernelEvalCache(i1, i1),
              kernelEvalCache(i1, i2),
              kernelEvalCache(i2, i2)
            )
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

          if (newAlpha2.isNaN)
            false

          else if (math.abs(newAlpha2 - alph2) < tolerance * (newAlpha2 + alph2 + tolerance))
            false

          else {

            val newAlpha1 = alph1 + s * (alph2 - newAlpha2)

            if (!newAlpha1.isNaN) {

              // store the new, calculated lagrange multipliers
              alphas(i1) = newAlpha1
              alphas(i2) = newAlpha2

              println(
                s"""${"\t"}UPDATING
                   |${"\t"}alphas($i1): ${alphas(i1)} (old: $alph1)
                   |${"\t"}alphas($i2): ${alphas(i2)} (old: $alph2)""".stripMargin
              )

              // calculate the threshold update
              b =
                if (0.0 < newAlpha1 && newAlpha1 < C) {
                  val b1 = b - e1 - y1 * (newAlpha1 - alph1) * k11 - y2 * (newAlpha2 - alph2) * k12
                  b1

                } else if (0.0 < newAlpha2 && newAlpha2 < C) {
                  val b2 = b - e2 - y1 * (newAlpha1 - alph1) * k12 - y2 * (newAlpha2 - alph2) * k22
                  b2

                } else {
                  val a1DiffY1 = y1 * (newAlpha1 - alph1)
                  val a2DiffY2 = y2 * (newAlpha2 - alph2)

                  val b1 = b - e1 - a1DiffY1 * k11 - a2DiffY2 * k12
                  val b2 = b - e2 - a1DiffY1 * k12 - a2DiffY2 * k22

                  0.5 * (b1 + b2)
                }

              // update our error cache with the SVM predictions on i1,i2 using
              // the updated alphas
              errorCache(i1) = predict(i1) - y1
              errorCache(i2) = predict(i2) - y2

              // yes we changed alphas!
              true

            } else
              false
          }
        }
      }

    def examineExample(index: Int): Int = {

      val y2 = targetOnly(index)
      val alph2 = alphas(index)
      val e2 = errorCache(index)
      val r2 = e2 * y2

      if ((r2 < -tolerance && alph2 < C) || (r2 > tolerance && alph2 > 0.0)) {

        // second choice heuristic
        val nonBoundExamples = calculateNonBoundExamples()
        if (nonBoundExamples nonEmpty) {

          val secondChoiceAlphaIndex =
            if (e2 > 0.0) {

              var indexOfMinError = 0
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
              var indexOfMaxError = 0
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

            } else if (config.doFullAlphaSearch) {
              // 2nd tier, second choice heuristics: option #2
              val randomIndex = randomExample(size, index)

              var changedAnAlpha = false
              var nonBoundIndex = 0
              cfor(0)(_ < size && !changedAnAlpha, _ + 1) { i =>
                val nextIndex = (randomIndex + i) % size

                val indexOfANonBoundExample =
                  if (nonBoundIndex < nonBoundExamples.length)
                    nonBoundExamples(nonBoundIndex)
                  else
                    -1

                if (nextIndex != indexOfANonBoundExample && nextIndex != index) {
                  changedAnAlpha = takeStep(nextIndex, index)

                } else {
                  // we've already tried to take a step at `nextIndex`
                  // so let's not do it again!
                  nonBoundIndex += 1
                }
              }

              if (changedAnAlpha) {
                1

              } else {
                0
              }

            } else {
              0
            }
          }
        } else {
          0
        }
      } else {
        0
      }
    }

    System.gc()

    //
    // MAIN LOOP --> heuristic for selecting alpha_1
    //

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
          val indexToExamine = nonBoundExamples(i)
          numChanged += examineExample(indexToExamine)
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

    val (nonZeroBothAlphaTarget, vectorsForNZA) = {

      val inidicesOfNonZeroAlphas: Seq[Int] =
        alphas
          .data
          .toSeq
          .zipWithIndex
          .filter { case (a, _) => !a.isNaN && !isZero(a) }
          .map { case (_, index) => index }

      val (bothATNZ, v4NZA) = {
        val s = inidicesOfNonZeroAlphas.size
        (new Array[Double](s), new Array[Vec](s))
      }

      cfor(0)(_ < inidicesOfNonZeroAlphas.size, _ + 1) { i =>

        val indexOfSupportVector = inidicesOfNonZeroAlphas(i)

        println(s"\talphas($indexOfSupportVector): ${alphas(indexOfSupportVector)}")

        bothATNZ(i) = alphas(indexOfSupportVector) * targetOnly(indexOfSupportVector)
        v4NZA(i) = vecOnly(indexOfSupportVector)
      }

      (bothATNZ, v4NZA.toIndexedSeq)
    }

    SvmDualModel(
      bothAlphaTargets = nonZeroBothAlphaTarget,
      supportVectors = vectorsForNZA,
      b = if (!b.isNaN) b else 0.0,
      K = K
    )
  }

}