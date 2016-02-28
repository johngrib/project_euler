/**
 * http://euler.synap.co.kr/prob_detail.php?id=23
 *
 * 자신을 제외한 약수(진약수)를 모두 더하면 자기 자신이 되는 수를 완전수라고 합니다.
 * 예를 들어 28은 1 + 2 + 4 + 7 + 14 = 28 이므로 완전수입니다.
 * 또, 진약수의 합이 자신보다 작으면 부족수, 자신보다 클 때는 초과수라고 합니다.
 *
 * 12는 1 + 2 + 3 + 4 + 6 = 16 > 12 로서 초과수 중에서는 가장 작습니다.
 * 따라서 초과수 두 개의 합으로 나타낼 수 있는 수 중 가장 작은 수는 24 (= 12 + 12) 입니다.
 *
 * 해석학적인 방법을 사용하면, 28123을 넘는 모든 정수는 두 초과수의 합으로 표현 가능함을 보일 수가 있습니다.
 * 두 초과수의 합으로 나타낼 수 없는 가장 큰 수는 실제로는 이 한계값보다 작지만, 해석학적인 방법으로는 더 이상 이 한계값을 낮출 수 없다고 합니다.
 *
 * 그렇다면, 초과수 두 개의 합으로 나타낼 수 없는 모든 양의 정수의 합은 얼마입니까?
 * 
 * 2015.09.01 johngrib82@gmail.com
 */
object Problem {

  def main(args: Array[String]) {

    import scala.collection.mutable.Set

    // 초과수인지 확인한다
    def isAbundant(num: Int): Boolean = {
      val ls: Set[Int] = Set()
      for { a <- 1 to num; if (num % a == 0) }
        ls += (a, num / a)
      (ls - num).sum > num
    }

    // 초과수들의 덧셈 조합을 구한다
    def getAbundantsComb(ab: List[Int], numbers: Array[Boolean]): Array[Boolean] = {
      if (ab.length < 1)
        numbers
      else {
        ab.foreach((x) => numbers(x + ab.head) = true)
        getAbundantsComb(ab.tail, numbers) // 재귀
      }
    }

    val abundants = (for { i <- 1 until 28123; if (isAbundant(i)) } yield i).toList
    val numbers = getAbundantsComb(abundants, new Array[Boolean](28123 * 2))

    val result = (for { i <- 1 until numbers.length; if (!numbers(i) && i <= 28123) }
      yield i).toList.sum
    println(result)
  }
}
