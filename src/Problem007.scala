/**
 * http://euler.synap.co.kr/prob_detail.php?id=7 
 * 
 * 소수를 크기 순으로 나열하면 2, 3, 5, 7, 11, 13, ... 과 같이 됩니다. 이 때 10,001번째의 소수를 구하세요.
 * 
 * 2015.08.24 johngrib82@gmail.com
 */
object Problem007 {
  def main(args: Array[String]) {

    def getPrimes(primes: List[Int], n: Int, count: Int): List[Int] = {

      val newList =
        if (primes exists (n % _ == 0)) primes
        else primes ::: List(n)

      if (primes.length >= count) primes
      else getPrimes(newList, n + 2, count);   // recursion
    }

    val primeList = getPrimes(List(2), 3, 10001);
    print(primeList.last)
  }
}