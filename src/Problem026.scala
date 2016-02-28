/**
 * http://euler.synap.co.kr/prob_detail.php?id=26
 *
 * 분자가 1인 분수를 단위분수라고 합니다. 분모가 2에서 10까지의 단위분수는 아래와 같습니다.
 *
 * 숫자 위에 찍힌 점은 순환마디를 나타내는데, 1/6의 경우 순환마디는 "6"으로 0.166666...처럼 6이 무한히 반복됨을 뜻합니다. 같은 식으로 1/7은 6자리의 순환마디(142857)를 가집니다.
 * d 를 1000 이하의 정수라고 할 때, 단위분수 1/d 의 순환마디가 가장 긴 수는 무엇입니까?
 *
 * 2015.09.02 johngrib82@gmail.com
 */
object Problem026 {

  def main(args: Array[String]) {

    def div(d: Int, s: Int, result: String, check: List[Int]): Int = {

      val re = s % d
      val newCheck = check ::: List(re)

      if (check exists (_ == re)) { // 만약 나눗셈의 결과가 이전에 구한 값과 똑같이 나온다면 중단하고 나머지의 리스트 길이를 리턴한다
        (check dropWhile (_ == re)).length + 1
      } else if (d > s) // 나누는 자릿수 이동
        div(d, s * 10, result + 0, newCheck) // 재귀
      else if (re > 0)
        div(d, re * 10, result + (s / d).toInt, newCheck) // 재귀
      else
        0
    }

    val result = (1 to 1000).toList.map((x) => (x, div(x, 1, "", List())))
    val rs = ((0, 0) /: result)((x, y) => if (x._2 > y._2) x else y)
    println(rs._1)
  }
}
// 수행시간 : 415 ms
