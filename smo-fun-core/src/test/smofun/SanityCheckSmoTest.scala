package smofun

class SanityCheckSmoTest extends FunSuite {

}

object SanityCheckSmoTest {


  val inputs = Seq(
    DenseVector(-7d, -4d),
    DenseVector(-9d, -8d),
    DenseVector(2d, 5d),
    DenseVector(-3d, -10d),
    DenseVector(9d, 7d),
    DenseVector(3d, 8d),
    DenseVector(8d, 11d),
    DenseVector(8d, 9d)
  )

  val targets = Seq(
    -1d,
      -1d,
      -1d,
      -1d,
  1d,
  1d,
  1d,
  1d
  )





 /*
    -7   -4
   -9   -8
    2    5
   -3  -10
    9    7
    3    8
    8   11
    8    9


  */

}