/**
 * http://euler.synap.co.kr/prob_detail.php?id=12
 * 
1부터 n까지의 자연수를 차례로 더하여 구해진 값을 삼각수라고 합니다.
예를 들어 7번째 삼각수는 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28이 됩니다.
이런 식으로 삼각수를 구해 나가면 다음과 같습니다.
1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
이 삼각수들의 약수를 구해봅시다.
 1: 1
 3: 1, 3
 6: 1, 2, 3, 6
10: 1, 2, 5, 10
15: 1, 3, 5, 15
21: 1, 3, 7, 21
28: 1, 2, 4, 7, 14, 28
위에서 보듯이, 5개 이상의 약수를 갖는 첫번째 삼각수는 28입니다.
그러면 500개 이상의 약수를 갖는 가장 작은 삼각수는 얼마입니까?
*/
object Problem012 {

  import scala.collection.mutable.Map

  def main(args: Array[String]) {

    var primes = List(2L, 3L, 5L);
    var factorSaved = Map.empty[Long, List[Long]];

    /**
     * 충분한 양의 소수를 확보한 다음, 소인수분해를 수행한다
     */
    def getFactor(number: Long): List[Long] = {

      // 다음 소수를 구하여 primes 에 추가한다
      def getNextPrime(primes: List[Long], n: Long): List[Long] = {
        if (primes exists (n % _ == 0))
          getNextPrime(primes, n + 2L); // recursion
        else
          primes ::: List(n)
      }
      // 충분한 양의 소수를 준비한다
      def getEnoughPrimes(number: Long): List[Long] = {
        if (primes.last < number) {
          primes = getNextPrime(primes, primes.last + 2L);
          getEnoughPrimes(number)
        } else
          primes
      }

      // 소인수분해를 수행한다
      def factorize(number: Long, 
                    originalNumber: Long, 
                    chkPrime: List[Long], 
                    result: List[Long]): List[Long] = {

        if (number < 2 || chkPrime.head > number) {
            // 소인수 분해 결과를 캐싱한다
            if (!(factorSaved contains originalNumber)) factorSaved(originalNumber) = result 
            return result
        // 캐싱된 소인수 분해인 경우 예전에 계산해 둔 결과를 리턴한다
        } else if (factorSaved contains number) { 
            val fResult: List[Long] = factorSaved(number) ::: result
            // 소인수 분해 결과를 캐싱한다
            if (!(factorSaved contains originalNumber)) factorSaved(originalNumber) = fResult 
            return fResult // 결과 리턴
        } else if (number % chkPrime.head == 0) {
            factorize(number / chkPrime.head, originalNumber, chkPrime, result ::: List(chkPrime.head))
        } else {
            factorize(number, originalNumber, chkPrime.tail, result)
        }
      }

      primes = getEnoughPrimes(number)
      factorize(number, number, primes, List())
    }

    /**
     * 약수의 개수를 구한다. 약수의 개수는 경우의 수를 고려하면 쉽게 공식화할 수 있다.
     * 약수의 개수는 소인수 분해를 수행한 후, 각 인수의 지수에 + 1 한 값을 곱하면 된다.
     */
    def getFactorCount(ls: List[Long]): Long = {
        var result = Map.empty[Long, Long]
        // 인수별 지수+1 의 값을 카운트한다.
        for (a <- ls) 
          if (result contains a)
              result(a) += 1
          else
              result(a) = 2
      result.toList.unzip._2.foldLeft(1L)(_ * _) // 약수의 개수
    }

    /**
     * 답을 구한다. 삼각수는 sigma k 이므로 n * (n + 1) / 2 를 이용한다. 
     * 이 때, n 과 n + 1 을 따로 소인수분해하면 삼각수 자체를 소인수분해하는 것보다 더 빠르게 소인수분해할 수 있다.
     */
    def calc(count: Long, isLeftOdd: Boolean): Long = {
      val left: Long = if (isLeftOdd) count else count / 2;
      val right: Long = if (isLeftOdd) (count + 1) / 2 else count + 1;
      val fac = getFactor(left) ::: getFactor(right) // 소인수 분해 결과

      if (getFactorCount(fac) < 500)  // 약수의 개수가 500 개 미만이라면 재귀
        calc(count + 1, !isLeftOdd)
      else
        left * right
    }

    val start = System.currentTimeMillis();
    println(calc(3, true))  // 3 번째 삼각수부터 시작한다
    val end = System.currentTimeMillis();
    println("소요시간 ", end - start)    
  }
}
