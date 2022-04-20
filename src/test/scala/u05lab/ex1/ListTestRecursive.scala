package u05lab.ex1

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test
import u05lab.ex1.List.*

class ListTestRecursive {

  private val l = List(1, 2, 3, 4)

  @Test def testZipRight(): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), l.zipRightR)

  @Test def testZipRightEmpty(): Unit =
    assertEquals(Nil(), Nil().zipRightR)

  @Test def testPartition(): Unit =
    assertEquals((List(2, 4), List(1, 3)), l.partitionR(_ % 2 == 0))

  @Test def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 4)), l.spanR(_ % 2 != 0))

  @Test def testReduce(): Unit =
    assertEquals(10, l.reduceR(_ + _))

  @Test def testReduceEmpty(): Unit =
    assertThrows(classOf[UnsupportedOperationException], () => Nil().reduceR((a,b) => a))

  @Test def testTakeRight(): Unit =
    assertEquals(List(2, 3, 4), l.takeRightR(3))

  @Test def testTakeRightEmpty(): Unit =
    assertEquals(Nil(), Nil().takeRightR(3))

  @Test def testCollect(): Unit =
    assertEquals(List(2, 3, 4), l.collectR({case i if i < 4 => i + 1}))
}
