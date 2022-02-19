package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean =
    chars.foldLeft(0) {
      (count, ch) => ch match
        case '(' => if count < 0 then -1 else count + 1
        case ')' => if count < 0 then -1 else count - 1
        case _ => count
    } == 0

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      chars.slice(idx, until).foldLeft((0, 0)) {
        (data, ch) =>
          val min = data._1
          val bal = data._2
          ch match
            case '(' => (min, bal + 1)
            case ')' => (Math.min(min, bal - 1), bal - 1)
            case _ => (min, bal)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from < threshold) then
        traverse(from, until, 0, 0)
      else
        val mid = from + (until - from) / 2
        val ((minL, balL), (minR, balR)) = parallel(reduce(from, mid), reduce(mid, until))
        (Math.min(minL, balL - minR), balL + balR)
    }

    reduce(0, chars.length) == (0, 0)

  // For those who want more:
  // Prove that your reduction operator is associative!

