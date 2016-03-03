/*
 * http://euler.synap.co.kr/prob_detail.php?id=32
 * 
 * 1부터 n까지의 각 숫자를 한번씩만 써서 만들 수 있는 숫자를 팬디지털(pandigital)이라고 합니다.
 * 예를 들면 15234는 1부터 5의 숫자가 한번씩만 쓰였으므로 1 ~ 5 팬디지털입니다.
 *
 * 7254라는 숫자는 그런 면에서 특이한데, 39 × 186 = 7254 라는 곱셈식을 만들 때 이것이 1 ~ 9 팬디지털이 되기 때문입니다.
 *
 * 이런 식으로 a × b = c 가 1 ~ 9 팬디지털이 되는 모든 c의 합은 얼마입니까?
 * 
 * (참고: 어떤 c는 두 개 이상의 (a, b)쌍에 대응될 수도 있는데, 이런 경우는 하나로 칩니다)
 *
 * 2016.03.03 johngrib82@gmail.com
 */
object Problem032 {

  def main(args: Array[String]): Unit = {

    def isPandigit(numStr: String): Boolean = {
      numStr.size == 9 && "123456789".forall(numStr.contains(_))
    }

    def getResult(aStart: Int, aEnd: Int, bStart: Int, bEnd: Int): Int = {
      (for {
        a <- aStart to aEnd
        b <- bStart to bEnd
        c = a * b
        str = List(a, b, c).mkString
        if (isPandigit(str))
      } yield c).distinct.sum
    }

    val time = System.currentTimeMillis()
    // 1 자리 숫자 * 4자리 숫자
    val result1 = getResult(2, 9, 1000, 9999)

    // 2 자리 숫자 * 3 자리 숫자
    val result2 = getResult(10, 99, 100, 999)
    
    println(System.currentTimeMillis() - time)

    println(result1 + result2)
  }
}
