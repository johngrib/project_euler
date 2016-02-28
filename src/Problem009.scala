/**
 * http://euler.synap.co.kr/prob_detail.php?id=9
 * 
 * 세 자연수 a, b, c 가 피타고라스 정리 a2 + b2 = c2 를 만족하면 피타고라스 수라고 부릅니다 (여기서 a < b < c ).
 * 예를 들면 32 + 42 = 9 + 16 = 25 = 52이므로 3, 4, 5는 피타고라스 수입니다.
 * 
 *  a + b + c = 1000 인 피타고라스 수 a, b, c는 한 가지 뿐입니다. 이 때, a × b × c 는 얼마입니까?
 *  
 *  2015.08.24 johngrib82@gmail.com
*/

object Problem009 {
  
  def main(args: Array[String]) {

    for {
      a <- 1 to 1000;
      b <- (a + 1) to 1000
      c2 = a * a + b * b
      c:Double = math.sqrt(c2)
      if(c - c.toInt == 0 && a + b + c == 1000)
    } {
      println(a * b * c.toInt)
    }
  }
}