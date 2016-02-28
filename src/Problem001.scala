/**
 * http://euler.synap.co.kr/prob_detail.php?id=1
 * 10보다 작은 자연수 중에서 3 또는 5의 배수는 3, 5, 6, 9 이고, 이것을 모두 더하면 23입니다. 
 * 1000보다 작은 자연수 중에서 3 또는 5의 배수를 모두 더하면 얼마일까요?
 * 
 * 2015.07.31. johngrib82@gmail.com
 */
object Problem001 {
  def main(args: Array[String]) {

    val result =
      (1 until 1000)
        .toList
        .filter((n) => n % 3 == 0 || n % 5 == 0)
        .reduce((a, b) => a + b);

    println(result);
  }
}