package u05lab.ex1

import org.junit.Assert.assertThrows
import org.junit.Assert.assertEquals
import org.junit.Test
import u05lab.ex1.List.Nil

class ListTest {

  private val l = List(1, 2, 3, 3, 4)

  @Test def testAppend(): Unit =
    val l2 = List(5, 6)
    assertEquals(List(1, 2, 3, 3, 4, 5, 6), l.append(l2))

  @Test def testFilter(): Unit =
    assertEquals(List(3, 3, 4), l.filter(_ > 2))

  @Test def testMap(): Unit =
    assertEquals(List(3, 3, 4), l.filter(_ > 2))

  @Test def testZipRight(): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (3, 3), (4, 4)), l.zipRight)

  @Test def testZipRightEmpty(): Unit =
    assertEquals(Nil(), Nil().zipRight)

  @Test def testPartition(): Unit =
    assertEquals((List(2, 4), List(1, 3, 3)), l.partition(_ % 2 == 0))

  @Test def testPartitionFold(): Unit =
    assertEquals((List(2, 4), List(1, 3, 3)), l.partitionFold(_ % 2 == 0))

  @Test def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 3, 4)), l.span(_ % 2 != 0))

  @Test def testReduce(): Unit =
    assertEquals(13, l.reduce(_ + _))

  @Test def testReduceEmpty(): Unit =
    assertThrows(classOf[UnsupportedOperationException], () => Nil().reduce((a,b) => a))

  @Test def testTakeRight(): Unit =
    assertEquals(List(3, 3, 4), l.takeRight(3))

  @Test def testTakeRightEmpty(): Unit =
    assertEquals(Nil(), Nil().takeRight(3))

  @Test def testCollect(): Unit =
    assertEquals(List(2, 3, 4, 4), l.collect({case i if i < 4 => i + 1}))

}
