/**
 * http://euler.synap.co.kr/prob_detail.php?id=15
 *
 * 아래와 같은 2 × 2 격자의 왼쪽 위 모서리에서 출발하여 오른쪽 아래 모서리까지 도달하는 길은 모두 6가지가 있습니다 (거슬러 가지는 않기로 합니다).
 *
 * 그러면 20 × 20 격자에는 모두 몇 개의 경로가 있습니까?
 * 
 * 2015.08.28 johngrib82@gmail.com
 */

// 단순한 조합 문제다. 40C20 으로 해결 가능. 따라서 콤비네이션 펑션만 작성해주면 된다.

import scala.math.BigInt
object Problem015 {
  def main(args: Array[String]) {

    def combination(n: Int, k: Int): BigInt = {
      require(n > 0 && k >= 0)

      if (n < k || n < 1 || k < 0)
        0
      else if (n == k || k == 0)
        1
      else {
        val kk = if (n - k < k) n - k else k
        val nList = ((n to (1, -1)).toList take kk).map(BigInt(_)) // 분모
        val kList = (kk to (1, -1)).toList // 분자
        val up = (BigInt(1) /: nList)(_ * _) // 귀찮으니 그냥 BigInt 를 쓰자
        (up /: kList)(_ / _);
      }
    }

    val start = System.currentTimeMillis();
    val result = combination(40, 20);
    println("수행시간", System.currentTimeMillis() - start, "결과", result)
  }
}
