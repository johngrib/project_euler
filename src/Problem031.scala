/**
 * http://euler.synap.co.kr/prob_detail.php?id=31
 *
 * 영국의 화폐 단위는 파운드(£)와 펜스(p)이고, 동전의 종류는 아래와 같습니다.
 *
 * 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), £2 (200p)
 * 이 동전들을 가지고 2파운드를 만드는 방법은 다양할 것입니다. 예를 하나 들면 이렇습니다.
 *
 * 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
 * 2파운드를 만드는 서로 다른 방법은 모두 몇가지나 있습니까?
 *
 * 2016.03.02 johngrib82@gmail.com
 */
object Problem031 {

  def main(args: Array[String]): Unit = {

    def countCase(coins: List[Int], limit: Int): Int = {
      // 갖고 있는 동전이 하나도 없다면 만들 수 있는 경우의 수는 0 이다.
      // 갖고 있는 동전들 중 가장 작은 동전이 만들어야 하는 동전보다 크다면 만들 수 있는 경우의 수는 0 이다.
      if (coins.size < 1 || coins.head > limit)
        0
      // 갖고 있는 동전들 중 가장 작은 동전이 만들어야 하는 동전과 같은 크기라면 경우의 수는 1 이다.
      else if (coins.head == limit)
        1
      // 그 외의 경우, 만들어야 하는 금액보다 한 단계 작은 금액을 만드는 경우의 수와, 
      // 갖고 있는 동전 중 가장 작은 동전을 제외하고 만들 수 있는 경우의 수의 합이다.
      else
        countCase(coins, limit - coins.head) + countCase(coins.tail, limit)
    }

    val result = countCase(List(1, 2, 5, 10, 20, 50, 100, 200), 200)
    println(result);

  }
}
