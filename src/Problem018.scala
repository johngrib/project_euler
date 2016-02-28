/**
 * http://euler.synap.co.kr/prob_detail.php?id=18
 *
 * 다음과 같이 삼각형 모양으로 숫자를 배열했습니다.
 *    3
 *   7 4
 *  2 4 6
 * 8 5 9 3
 *
 * 삼각형의 꼭대기부터 아래쪽으로 인접한 숫자를 찾아 내려가면서 합을 구하면, 위의 그림처럼 3 + 7 + 4 + 9 = 23 이 가장 큰 합을 갖는 경로가 됩니다.
 * 다음 삼각형에서 합이 최대가 되는 경로를 찾아서 그 합을 구하세요.
  *                              75
  *                            95  64
  *                          17  47  82
  *                        18  35  87  10
  *                      20  04  82  47  65
  *                    19  01  23  75  03  34
  *                  88  02  77  73  07  63  67
  *                99  65  04  28  06  16  70  92
  *              41  41  26  56  83  40  80  70  33
  *            41  48  72  33  47  32  37  16  94  29
  *          53  71  44  65  25  43  91  52  97  51  14
  *        70  11  33  28  77  73  17  78  39  68  17  57
  *      91  71  52  38  17  14  91  43  58  50  27  29  48
  *    63  66  04  68  89  53  67  30  73  16  69  87  40  31
  *  04  62  98  27  23  09  70  98  73  93  38  53  60  04  23
 *
 * 참고: 여기서는 경로가 16384개밖에 안되기 때문에, 모든 경로의 합을 일일이 계산해서 답을 구하는 것이 가능합니다.
 * 하지만 67번 문제에는 100층짜리 삼각형 배열이 나옵니다. 그런 경우에는 좀 더 현명한 풀이 방법을 찾아야겠지요.
 * 
 * 2015.08.31 johngrib82@gmail.com
 */
object Problem018 {

  def main(args: Array[String]) {
    val numbers = """       |75
                          |95  64
                        |17  47  82
                      |18  35  87  10
                    |20  04  82  47  65
                  |19  01  23  75  03  34
                |88  02  77  73  07  63  67
              |99  65  04  28  06  16  70  92
            |41  41  26  56  83  40  80  70  33
          |41  48  72  33  47  32  37  16  94  29
        |53  71  44  65  25  43  91  52  97  51  14
      |70  11  33  28  77  73  17  78  39  68  17  57
    |91  71  52  38  17  14  91  43  58  50  27  29  48
  |63  66  04  68  89  53  67  30  73  16  69  87  40  31
|04  62  98  27  23  09  70  98  73  93  38  53  60  04  23"""
      .stripMargin.trim.split("\n").toList.map(_.split("\\s+").toList.map(_.toLong))

    // 위에서부터 덧셈하며 내려간다. 그러므로 최종 단계에서 max 값을 구하면 된다.
    def findMax(lsUp: List[Long], lsDown: List[List[Long]]): Long = {
      if (lsDown.length < 1) return lsUp.max

      val up = List(0L) ::: lsUp ::: List(0L)
      val down = lsDown.head

      val result =
        (for (i <- 0 until down.length)
          yield List(up(i), up(i + 1)).max + down(i)).toList

      //println(result)
      findMax(result, lsDown.tail) // 재귀
    }

    val start = System.currentTimeMillis()
    println(findMax(numbers.head, numbers.tail))
    println(System.currentTimeMillis() - start)
  }
}
