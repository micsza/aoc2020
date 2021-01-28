package solutions

import scala.annotation.tailrec

object Day25 {

  object part1 {

    def key(subjectNumber: Long = 7, loopSize: Long): Long = {

      @tailrec
      def go(n: Long, acc: Long): Long = {
        if (n > 1) go(n-1, (acc * subjectNumber) % 20201227)
        else acc
      }

      go(loopSize, subjectNumber)
    }

    def publicKey(loopSize: Long) = key(subjectNumber = 7, loopSize = loopSize)

    def decryptLoopSize(key: Long): Long = {

      @tailrec
      def go(l: Long, accKey: Long): Long = {
        if (accKey == key) l
        else go(l+1, (accKey * 7) % 20201227)
      }

      go(0, 1)
    }
  }
}

object Day25App extends App {
  import Day25._

  val res = (1 to 12).toList.map {m => (m, part1.publicKey(loopSize = m.toLong))}
  println(res)

  val testKey1 = 5764801
  val l1 = part1.decryptLoopSize(testKey1)
  println(l1)

  val testKay2 = 17807724
  val l2 = part1.decryptLoopSize(testKay2)
  println(l2)

  val testEncKey = part1.key(testKey1, l2)
  println(testEncKey)

  val publicKey1 = 8252394L
  val publicKey2 = 6269621L

  val loopSize1 = part1.decryptLoopSize(publicKey1)
  println(s"loop size 1 = $loopSize1")
  val encKey = part1.key(publicKey2, loopSize1)
  println(s"enc key = $encKey")

}
