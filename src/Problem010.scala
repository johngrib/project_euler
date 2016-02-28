/**
 * http://euler.synap.co.kr/prob_detail.php?id=10
 *
 * 10 이하의 소수를 모두 더하면 2 + 3 + 5 + 7 = 17 이 됩니다.
 * 이백만(2,000,000) 이하 소수의 합은 얼마입니까?
 *
 *
 */
object Problem010 {
  def main(args: Array[String]) {
    val primes = getPrimes(List(2L), 3L, 2000000L)
    val sum: Long = primes.sum
    println(primes)
    println(sum)
  }
  // 에라토스테네스의 체를 사용하여 개선할 수 있... 지만 귀찮으니 나중에 하자.
  def getPrimes(ls: List[Long], number: Long, max: Long): List[Long] = {
    if (number > max)
      ls
    else if (ls exists (number % _ == 0)) // if number is not a prime
      getPrimes(ls, number + 2L, max);
    else // if number is prime
      getPrimes(ls ::: List(number), number + 2L, max)
  }
}