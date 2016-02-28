/**
 * http://euler.synap.co.kr/prob_detail.php?id=24
 *
 * 어떤 대상을 순서에 따라 배열한 것을 순열이라고 합니다. 예를 들어 3124는 숫자 1, 2, 3, 4로 만들 수 있는 순열 중 하나입니다.
 * 이렇게 만들 수 있는 모든 순열을 숫자나 문자 순으로 늘어놓은 것을 사전식(lexicographic) 순서라고 합니다.
 * 0, 1, 2로 만들 수 있는 사전식 순열은 다음과 같습니다.
 *
 * 012   021   102   120   201   210
 * 0, 1, 2, 3, 4, 5, 6, 7, 8, 9로 만들 수 있는 사전식 순열에서 1,000,000번째는 무엇입니까?
 *
 * 2015.09.02 johngrib82@gmail.com
 */
object Problem024 {

  def main(args: Array[String]) {

    // a * 9! + b * 8! + c * 7! + .... 이 1,000,000 에 접근하게 하면 됨 

    // 팩토리얼 계산 함수
    def getFactorial(num: Int, result: Int): Int =
      if (num <= 1) result
      else getFactorial(num - 1, result * num)

    // count + 1 번째 숫자를 찾아내는 함수
    def calc(seq: Int, remain: List[Int], result: List[Int]): String =
      if (remain.length < 1)
        result.mkString
      else {
        val num = getFactorial(remain.length - 1, 1) // 이 숫자가 현재 위치에 있을 경우 조합할 수 있는 숫자의 수
        val nextNumber = remain(seq / num) // 위의 계산을 토대로 남아있는 수 중에서 몇 번째 수가 적합한지 선택한다 
        calc(seq % num, remain.filter(_ != nextNumber), result ::: List(nextNumber)) // 재귀
      }

    val start = System.currentTimeMillis()
    val rs = calc(1000000 - 1, (0 to 9).toList, List())
    println(System.currentTimeMillis() - start)
    println(rs)

  }
}
/*
// 이런 방법도 있네;
val result = "0123456789".permutations.toList.sorted
Console println s"${result(999999)}"
*/