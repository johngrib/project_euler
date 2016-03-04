/*
 * http://euler.synap.co.kr/prob_detail.php?id=33
 * 
 * 분수 49/98에는 재미있는 성질이 있습니다. 수학을 잘 모르는 사람이 분모와 분자에서 9를 각각 지워서 간단히 하려고 49/98 = 4/8 처럼 계산해도 올바른 결과가 됩니다.
 *
 * 이에 비해 30/50 = 3/5 같은 경우는 다소 진부한 예라고 볼 수 있습니다.
 * 
 * 위와 같은 성질을 가지면서 '진부하지 않은' 분수는, 값이 1보다 작고 분자와 분모가 2자리 정수인 경우 모두 4개가 있습니다.
 *
 * 이 4개의 분수를 곱해서 약분했을 때 분모는 얼마입니까?
 *
 * 2016.03.05 johngrib82@gmail.com
 */
object Problem033 {

  def main(args: Array[String]): Unit = {

    def getGCD(a: Int, b: Int): Int =
      if (b == 0) a
      else getGCD(b, a % b)

    /*
     * 분자 : (10 a + b)
     * 분모 : (10 b + c)
     * (단, a == b 이면 약분이 안 되고, a > b 이면 가분수가 되므로, b > a 여야 한다.)
     *
     * 주어진 규칙에 따라 식으로 만들어 보면
     *
     * (10 a + b) / (10 b + c) = a / c
     *
     * c (10 a + b) = a (10 b + c)
     *
     * 10 ac + bc = 10 ab + ac
     *
     * c ( 9 a + b ) = 10 ab
     * 
     * 이를 만족하는 분자, 분모 쌍을 찾으면 된다.
     *
     */
    val rs = for (
      a <- 1 to 9;
      b <- (a + 1) to 9; // 11*n/11*n 형태를 방지한다
      c <- 1 to 9;
      if c * (9 * a + b) == 10 * a * b;
      numerator = 10 * a + b;
      denominator = 10 * b + c
    ) yield List(numerator, denominator)

    val numMulti = rs.map(x => x(0)).reduce(_ * _)     // 분자의 곱
    val denMulti = rs.map(x => x(1)).reduce(_ * _)     // 분모의 곱
    val result = denMulti / getGCD(numMulti, denMulti) // 약분 후 분모

    println(result)
  }
}
