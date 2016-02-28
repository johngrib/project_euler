/**
 * http://euler.synap.co.kr/prob_detail.php?id=27
 *
 * 오일러는 다음과 같은 멋진 2차식을 제시했습니다.
 * n2 + n + 41
 * 이 식의 n에다 0부터 39 사이의 숫자를 넣으면, 그 결과는 모두 소수가 됩니다.
 * 하지만 n = 40일 때의 값 402 + 40 + 41 은 40×(40 + 1) + 41 이므로 41로 나누어지고, n = 41일 때 역시 412 + 41 + 41 이므로 소수가 아닙니다.
 * 컴퓨터의 발전에 힘입어 n2 − 79n + 1601 이라는 엄청난 2차식이 발견되었는데, 이것은 n이 0에서 79 사이일 때 모두 80개의 소수를 만들어냅니다. 이 식의 계수의 곱은 -79 × 1601 = -126479가 됩니다.
 * 아래와 같은 모양의 2차식이 있다고 가정했을 때,
 * n2 + an + b   (단 | a | < 1000, | b | < 1000)
 * 0부터 시작하는 연속된 n에 대해 가장 많은 소수를 만들어내는 2차식을 찾아서, 그 계수 a와 b의 곱을 구하세요.
 *
 * 2015.09.04 johngrib82@gmail.com
 */
object Problem027 {

  import scala.collection.mutable.Map
  def main(args: Array[String]) {

    /*
     1. n 이 0 인 경우를 생각해보면 b 는 소수여야 한다
     2. n 이 1 인 경우를 생각해보면 1 + a + b 이고, 가장 작은 소수는 2 이므로, 1 + a + b >= 2 이어야 한다.
              따라서 a 의 최소값은 1 - b 이다.
     3. b 가 2 인 경우는 n 이 2 일 경우 조건을 만족하지 못하므로, b 의 최소값은 일단 3 으로 생각한다.
     4. n = b 인 경우 합성수가 되므로 n 의 최대값은 b - 1 이다.
     5. n 이 홀수인 경우와 n 이 짝수인 경우를 생각해 보면, a 가 짝수인 경우 식의 결과가 짝수가 되므로 a 는 홀수여야 한다.
     */

    // 에라토스테네스의 체
    def getPrimes(max: Int): List[Int] = {
      val eratosArr = new Array[Boolean](max)
      var primes: List[Int] = List()
      for (i <- 2 to max; j <- i to (max, i); if (j < max))
        if (i == j && !eratosArr(i))
          primes = primes ::: List(i)
        else
          eratosArr(j) = true

      primes
    }

    val primes = getPrimes(1500)

    // 주어진 공식
    def fn(b: Int)(a: Int)(n: Int) = n * (n + a) + b

    // a, b 에 대하여 가장 큰 n 값을 찾는 함수
    def findMaxN(a: Int, b: Int, n: Int, nMax: Int): Int = {
      if (n >= nMax || !(primes exists (_ == fn(b)(a)(n))))
        n
      else
        findMaxN(a, b, n + 1, nMax)
    }

    // 답을 찾는 함수 
    def calc(bList: List[Int], max: (Int, Int, Int)): Int = {

      val aMin = if ((1 - bList.head) % 2 == 0) 2 - bList.head else 1 - bList.head
      var (maxCount, maxCount_a) = (-1, aMin)

      for (
        a <- aMin to (999, 2);
        count = findMaxN(a, bList.head, 1, bList.head - 1); // (a, b, n, nMax)
        if (count >= maxCount)
      ) {
        maxCount = count
        maxCount_a = a
      }

      if (bList.length >= 1 && bList.head < 999)
        calc(bList.tail, if (maxCount > max._3) (maxCount_a, bList.head, maxCount) else max) // 재귀
      else
        max._1 * max._2
    }

    println(calc(primes, (0, 0, 0)))
  }
}
// 수행시간 : 217 ms
