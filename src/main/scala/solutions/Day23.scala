package solutions

import solutions.Day23.domain.Cycle

import scala.annotation.tailrec

object Day23 {

  object domain {

    type Label = Int

    case class Cycle(var head: Cup, var tail: Cup, var maxLabel: Int = 9) {

      val label2Cup: Map[Label, Cup] = {
        @tailrec
        def go(cup: Cup, acc: Map[Label, Cup]): Map[Label, Cup] = {
          if (cup != tail) go(cup.next, acc.updated(cup.data, cup))
          else acc.updated(cup.data, cup)
        }
        go(head, Map.empty)
      }

      def minLabel: Label = 1
//      def maxLabel: Label = 1000000//9

      def printMe(): Unit = {
        println(s"\n-- Cycle: head = ${head.data}, tail = ${tail.data}, tail next data = ${tail.next.data}")

        @tailrec
        def go(node: Cup): Cup = {
          print(s"${node.data} ~> ")
          if (node != tail) go(node.next)
          else node
        }
        val last = go(head)
        print(last.next.data)
      }

      def tickClockwise(): Unit = {
        tail = head
        head = head.next
      }

      def cutOut: (Cup, Cup) = {
        val cutHead = head.next
        val cutTail = cutHead.next.next
        head.next = cutTail.next
        cutTail.next = null
        (cutHead, cutTail)
      }

      def seqLabels(cutHead: Cup): List[Label] = {

        @tailrec
        def go(node: Cup, acc: List[Label]): List[Label] = {
          if (node.next == null) node.data :: acc
          else go(node.next, node.data :: acc)
        }

        go(cutHead, Nil)
      }

      def pasteInto(destination: Cup, pasteEnds: (Cup, Cup)): Unit = {
        val nodeToPaste = destination.next
        destination.next = pasteEnds._1
        pasteEnds._2.next = nodeToPaste
      }

      def cupOfLabel(target: Label): Cup = {
//        println(s"Looking for a cup of label $target")
//        Thread.sleep(100)
        @tailrec
        def go(node: Cup): Cup = {
//          println(s"Checking node with data = ${node.data} vs target = $target ...")
//          Thread.sleep(100)
          if (node.data == target) {
//            println(s"\t-> FOUND! ${node.data}")
            node
          }
          else {
//            println(s"\tNo match. Next one.")
            go(node.next)
          }
        }
        go(head)
      }

      def destinationCup(excludedLabels: List[Label]): Cup = {

        @tailrec
        def destinationLabel(target: Label): Label= {
          if (target < minLabel) destinationLabel(maxLabel)
          else {
            if (excludedLabels contains target) destinationLabel(target - 1)
            else target
          }
        }

        val destLab: Label = destinationLabel(head.data - 1)
        label2Cup(destLab)
      }

      def getTwoAfterOne: (Label, Label) = {
        val one = cupOfLabel(1)
        (one.next.data, one.next.next.data)
      }

      @tailrec
      final def move(n: Int): Unit = {

        if (n % 100000 == 0) println(s"******* move($n)")
//        printMe()
        if (n > 0) {
          val (cutHead, cutTail) = cutOut
//          Thread.sleep(100)
//          println(s"Cut: ${cutHead.data} .. ${cutTail.data}")
          val excluded = seqLabels(cutHead)
//          Thread.sleep(100)
//          println(s"Excluded: $excluded")
          val dest = destinationCup(excluded)
//          println(s"Dest = ${dest.data}")
          pasteInto(dest, (cutHead, cutTail))
          tickClockwise()
          move(n-1)
        }
      }
    }

    sealed case class Cup(var data: Label, var next: Cup) {
      def getData: Label = this.data

      def getNext: Cup = this.next

    }

    object Cycle {
      def apply(as: List[Label]): Cycle = {

        val nodes = as map {a => Cup(a, null)}

        for {
          (node, next) <- nodes zip nodes.drop(1)
        } node.next = next

        nodes.last.next = nodes.head

        new Cycle(nodes.head, nodes.last, as.max)
      }
    }

  }

}

object Day23App extends App {
  val testInput = List(3,8,9,1,2,5,4,6,7)
  val puzzleInput = List(9,2,5,1,7,6,8,3,4)

  val testInput2 = List(3,8,9,1,2,5,4,6,7) ++ (10 to 1000000).toList
  val puzzleInput2 = puzzleInput ++ (10 to 1000000).toList

//  val testCycle = Cycle(testInput)
//  testCycle.move(10)
//  testCycle.printMe()
//
//
//  val puzzleCycle = Cycle(puzzleInput)
//  puzzleCycle.move(100)
//  println()
//  puzzleCycle.printMe()
//  println(puzzleCycle.getTwoAfterOne)
//  val m = puzzleCycle.label2Cup
//  m foreach(p => println(p._1,p._2.data))


  println(s"--- Part 2 ---")
  val n = 10000000
  val testCycle2 = Cycle(testInput2)
  testCycle2.move(n)

  val puzzleCycle2 = Cycle(puzzleInput2)
  puzzleCycle2.move(n)
  println(puzzleCycle2.getTwoAfterOne)
  println(s"head = ${puzzleCycle2.head.data}, tail = ${puzzleCycle2.tail.data}, max = ${puzzleCycle2.maxLabel}")


}
