
    The code of the numerically unstable version is below:
    ```
        @Deprecated
        lazy val rbf_OLD_numerical_precision_problem: Gamma => Kernel =
          gamma => {
            assert(gamma > 0.0)
            val precomp = -gamma
            (v1, v2) => {
              val diff = v1 - v2
              math.exp(
                precomp * math.sqrt(diff.dot(diff))
              )
            }
          }
    ```

    Note that this follows the _exact_ definition of the rbf kernel ( exp {
    -gamma * || v1 - v2 ||^2 ). A more stable, equivalent calculation is
    what the code now uses:
    ```
       lazy val rbf: Gamma => Kernel =
          gamma => {
            assert(gamma > 0.0)
            val negGamma = -gamma
            (v1, v2) => {
              val x = negGamma * (v1.dot(v1) + v2.dot(v2) - 2.0 *
    v1.dot(v2))
              math.expm1(x) + 1.0
            }
          }
    ```

    Note that || v1 ||^2 + || v1 ||^2 - 2 * (v1 . v2) === || v1 - v2 ||^2
