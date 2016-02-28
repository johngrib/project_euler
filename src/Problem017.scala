/**
 * http://euler.synap.co.kr/prob_detail.php?id=17
 *
 * 1부터 5까지의 숫자를 영어로 쓰면 one, two, three, four, five 이고,
 * 각 단어의 길이를 더하면 3 + 3 + 5 + 4 + 4 = 19 이므로 사용된 글자는 모두 19개입니다.
 *
 * 1부터 1,000까지 영어로 썼을 때는 모두 몇 개의 글자를 사용해야 할까요?
 *
 * 참고: 빈 칸이나 하이픈('-')은 셈에서 제외하며, 단어 사이의 and 는 셈에 넣습니다.
 * 예를 들어 342를 영어로 쓰면 three hundred and forty-two 가 되어서 23 글자,
 * 115 = one hundred and fifteen 의 경우에는 20 글자가 됩니다.
 *
 * 2015.08.29 johngrib82@gmail.com
 */
object Problem017 {
  def main(args: Array[String]) {

    val dict = Map[Int, String]( // 글자의 length 를 보관한다.
      1 -> "one", 11 -> "eleven", 10 -> "ten",
      2 -> "two", 12 -> "twelve", 20 -> "twenty",
      3 -> "three", 13 -> "thirteen", 30 -> "thirty",
      4 -> "four", 14 -> "fourteen", 40 -> "forty",
      5 -> "five", 15 -> "fifteen", 50 -> "fifty",
      6 -> "six", 16 -> "sixteen", 60 -> "sixty",
      7 -> "seven", 17 -> "seventeen", 70 -> "seventy",
      8 -> "eight", 18 -> "eighteen", 80 -> "eighty",
      9 -> "nine", 19 -> "nineteen", 90 -> "ninety",
      100 -> "hundred", 1000 -> "onethousand").toList.map((x) => (x._1, x._2.length)).toMap

    def getNumberLength(n: Int): Int = {
      if (n == 1000) return dict(n)

      val (xoo, oxo, oox, oxx) = ((n % 1000) / 100, (n % 100) / 10, n % 10, n % 100)

      val pre =
        if (xoo > 0)
          dict(xoo) + dict(100) + (if (oxx > 0) 3 else 0) // 100 의 자리 숫자 표현 + and
        else 0

      val post =
        if (oxx <= 20 && oxx > 0) // 1~20 표현
          dict(oxx)
        else if (oxx > 20) // 20 보다 큰 경우 표현
          dict(oxo * 10) + (if (oox > 0) dict(oox) else 0)
        else 0

      pre + post
    }

    val rs2 = (1 to 1000).toList.map(getNumberLength _).sum
    println(rs2)
  }
}
