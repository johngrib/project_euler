/**
 * http://euler.synap.co.kr/prob_detail.php?id=30
 *
 * 각 자리의 숫자를 4제곱해서 더했을 때 자기 자신이 되는 수는 놀랍게도 단 세 개밖에 없습니다.
 * 1634 = 14 + 64 + 34 + 44
 * 8208 = 84 + 24 + 04 + 84
 * 9474 = 94 + 44 + 74 + 44
 * (1 = 14의 경우는 엄밀히 말해 합이 아니므로 제외합니다)
 * 위의 세 숫자를 모두 더하면 1634 + 8208 + 9474 = 19316 입니다.
 * 그렇다면, 각 자리 숫자를 5제곱해서 더했을 때 자기 자신이 되는 수들의 합은 얼마입니까?
 *
 * 2015.09.05 johngrib82@gmail.com
 */
object Problem030 {

  def main(args: Array[String]): Unit = {

    // 적절한 max 값을 구하는 것이 속도에 큰 영향을 준다.

    // 검사할 수의 max 값은 999... 가 9^5 * n 보다 커질 때를 기준으로 삼는다.
    def getMax(count: Int, nine5: Int): Int =
      if (nine5 * count < ("9" * count).toInt)
        nine5 * count
      else
        getMax(count + 1, nine5)

    // 0 부터 9 까지의 5 제곱 값을 미리 구해놓는다
    val pow5: List[Int] = (for (i <- 0 to 9) yield math.pow(i, 5).toInt).toList

    // 조건에 맞는 수를 찾는다
    val rs = for (
      i <- 2 to getMax(1, pow5(9));
      sum = i.toString.toList.map(_.toString.toInt).map(pow5(_)).sum;
      if (i == sum)
    ) yield i

    println(rs.sum)
  }
}
// 수행시간 : 455 ms
