/**
 * http://euler.synap.co.kr/prob_detail.php?id=28
 *
 * 숫자 1부터 시작해서 우측으로부터 시계방향으로 감아 5×5 행렬을 만들면 아래와 같이 됩니다.
 *
 * 여기서 대각선상의 숫자를 모두 더한 값은 101 입니다.
 * 같은 방식으로 1001×1001 행렬을 만들었을 때, 대각선상의 숫자를 더하면 얼마가 됩니까?
 * 
 * 2015.09.04 johngrib82@gmail.com
 */
// 1 +2 +2 +2 +2 +4 +4 +4 +4 +6 +6 +6 +6 +8 +8 +8 +8 … 을 풀어주면 된다.
object Problem028 {

  def main(args: Array[String]) {

    def calc(num: Int, plus: Int, count: Int, max: Int, result: Int): Int = {
      val nextNum = num + plus
      if (max < nextNum)
        result + 1 // 1 은 두 대각선에 공통으로 들어가므로 1 을 한 번 더 더해주어야 한다
      else if (count < 4)
        calc(nextNum, plus, count + 1, max, result + nextNum)
      else
        calc(nextNum, plus + 2, 1, max, result + nextNum)
    }

    println(calc(1, 2, 1, 1001 * 1001, 0))
  }
}
// 수행시간 : 36 ms 
