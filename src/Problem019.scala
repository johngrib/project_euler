/**
 * http://euler.synap.co.kr/prob_detail.php?id=19
 * 
 * 다음은 달력에 관한 몇 가지 일반적인 정보입니다 (필요한 경우 좀 더 연구를 해 보셔도 좋습니다).
 * 1900년 1월 1일은 월요일이다.
 * 4월, 6월, 9월, 11월은 30일까지 있고, 1월, 3월, 5월, 7월, 8월, 10월, 12월은 31일까지 있다.
 * 2월은 28일이지만, 윤년에는 29일까지 있다.
 * 윤년은 연도를 4로 나누어 떨어지는 해를 말한다. 하지만 400으로 나누어 떨어지지 않는 매 100년째는 윤년이 아니며, 400으로 나누어 떨어지면 윤년이다
 * 20세기 (1901년 1월 1일 ~ 2000년 12월 31일) 에서, 매월 1일이 일요일인 경우는 총 몇 번입니까?
 * 
 * 2015.08.31 johngrib82@gmail.com
 */
object Problem019 {
  def main(args: Array[String]) {

    val start = 1 // 1901년 1월 1일의 인덱스. 이 날은 화요일이다.
    val end = 365 * 100 + 100 / 4 // 2000년 12월 31일의 인덱스. 윤년이 100 / 4 만큼 있으므로 추가로 더해준다
    val month = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31); // 평년인 경우
    val leapM = List(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31); // 윤년인 경우
    val year = month ::: month ::: month ::: leapM

    def calc(today: Int, month: List[Int], countSunday: Int): Int = {

      if (today > end) return countSunday

      val isSunday: Boolean = today % 7 == 6
      val mList = if (month.length < 1) year else month
      calc(today + mList.head, mList.tail, countSunday + (if (isSunday) 1 else 0)) // 재귀
    }
    println(calc(start, year, 0))
  }
}
