package u05lab.ex3

import u05lab.ex3.Collections.PerformanceUtils.{MeasurementResults, measure}
import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

private val pw = PrintWriter("src/main/scala/u05lab/ex3/README.adoc")

object Collections:

  object PerformanceUtils:
    case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
      override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

    def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
      val startTime = System.nanoTime()
      val res = expr
      val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
      if (msg.nonEmpty) println(duration.toNanos + " nanos")
      MeasurementResults(res, duration)

    def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

  def printHeader(caption: String): Unit =
    pw.write("." + caption + "\n")
    pw.write("[stripes=even, cols=\"1,1\"]\n|===\n")
    pw.write("|Collection |Duration (microseconds)\n\n")
  def printLine[T](name: String)(expr: => T): Unit =
    pw.write("|" + name + "\n|" + measure(expr).duration.toMicros + "\n\n")
  def printFooter(): Unit = pw.write("|===\n\n")

@main def checkPerformance(): Unit =
  import Collections.*
  import scala.collection.mutable
  import scala.collection.mutable.{ArrayBuffer, ListBuffer}
  pw.write("= Performance comparison among Scala collections\n\n")

  val nElem = 100_000
  val l = List.range(1, nElem)
  val lb = ListBuffer.range(1, nElem)
  val v = Vector.range(1, nElem)
  val a = Array.range(1, nElem)
  val ab = ArrayBuffer.range(1, nElem)
  val s = Set.range(1, nElem)
  val ms = mutable.Set.range(1, nElem)

  /*
   *  Create
   */
  printHeader("Create, with " + nElem + " elements")
  printLine("List - Immutable")(List.range(1, nElem))
  printLine("List - Mutable")(ListBuffer.range(1, nElem))
  printLine("Vector - Immutable")(Vector.range(1, nElem))
  printLine("Array - Immutable")(Array(1 to nElem))
  printLine("Array - Mutable")(ArrayBuffer(1 to nElem))
  printLine("Set - Immutable")(Set(1 to nElem))
  printLine("Set - Mutable")(mutable.Set(1 to nElem))
  printFooter()

  /*
   * Read: Size
   */
  printHeader("Read: size")
  printLine("List - Immutable")(l.size)
  printLine("List - Mutable")(lb.size)
  printLine("Vector - Immutable")(v.size)
  printLine("Array - Immutable")(a.length)
  printLine("Array - Mutable")(ab.length)
  printLine("Set - Immutable")(s.size)
  printLine("Set - Mutable")(ms.size)
  printFooter()

  /*
   * Read: Get element
   */
  val getPos = nElem / 2
  printHeader("Read: Get element at position: " + getPos)
  printLine("List - Immutable")(l(getPos))
  printLine("List - Mutable")(lb(getPos))
  printLine("Vector - Immutable")(v(getPos))
  printLine("Array - Immutable")(a(getPos))
  printLine("Array - Mutable")(ab(getPos))
  printFooter()

  /*
   * Write: Add {head, tail}
   */
  printHeader("Write: Add element at {head, tail}")
  printLine("List - Immutable - Head")(0 :: l)
  printLine("List - Immutable - Tail")(l.:+(0))
  printLine("List - Mutable - Head")(lb prepend 0)
  printLine("List - Mutable - Tail")(lb append 0)
  printLine("Vector - Immutable - Head")(v.+:(1))
  printLine("Vector - Immutable - Tail")(v :+ 1)
  printLine("Array - Immutable - Head")(a.+:(1))
  printLine("Array - Immutable - Tail")(a.:+(1))
  printLine("Array - Mutable - Head")(ab prepend 1)
  printLine("Array - Mutable - Tail")(ab append 1)
  printFooter()

  /*
   * Remove
   */
  val takePos =  nElem / 2
  printHeader("Remove element from position: " + takePos)
  printLine("List - Immutable")(l take takePos)
  printLine("List - Mutable")(lb take takePos)
  printLine("Vector - Immutable")(v take takePos)
  printLine("Array - Immutable")(a take takePos)
  printLine("Array - Mutable")(ab take takePos)
  printFooter()

  pw.close()