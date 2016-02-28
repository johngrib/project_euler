/**
 * http://euler.synap.co.kr/prob_detail.php?id=16
 *
 * 215 = 32768 의 각 자리수를 더하면 3 + 2 + 7 + 6 + 8 = 26 입니다.
 * 21000 의 각 자리수를 모두 더하면 얼마입니까?
 * 
 * 2015.08.28 johngrib82@gmail.com
 */
object Problem016 {

  def main(args: Array[String]): Unit = {
    val result = BigInt(2).pow(1000).toString.toList.map(_.toString.toInt).sum
    println(result);
  }
}
