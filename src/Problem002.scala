/**
 * http://euler.synap.co.kr/prob_detail.php?id=2
 *
 * 피보나치 수열의 각 항은 바로 앞의 항 두 개를 더한 것이 됩니다.
 * 1과 2로 시작하는 경우 이 수열은 아래와 같습니다.
 * 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
 * 짝수이면서 4백만 이하인 모든 항을 더하면 얼마가 됩니까?
 *
 * 2015.07.31. johngrib82@gmail.com
 */
object Problem002 {
  def main(args: Array[String]) {

    val initFibo = List(1, 2)
    val limitNum = 4000000

    def getFiboList(numbers: List[Int], limit: Int): List[Int] = {

      val newNum = numbers.last + numbers.init.last

      if (newNum < limit)
        getFiboList(numbers ::: List(newNum), limit)
      else
        numbers
    }

    val result =
      getFiboList(initFibo, limitNum)
        .filter(_ % 2 == 0)
        .reduce(_ + _)

    println(result)
  }
}