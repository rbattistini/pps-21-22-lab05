package u05lab.ex1

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test
import u05lab.ex1.List.*

class ListTestRecursive {

  private val l = 1 :: 2 :: 3 :: 4 :: Nil()

  @Test def testZipRight(): Unit =
    assertEquals((1, 0) :: (2, 1) :: (3, 2) :: (4, 3) :: Nil(), l.zipRightR)

  @Test def testZipRightEmpty(): Unit =
    assertEquals(Nil(), Nil().zipRightR)

  @Test def testPartition(): Unit =
    assertEquals((2 :: 4 :: Nil(), 1 :: 3 :: Nil()), l.partitionR(_ % 2 == 0))

  @Test def testSpan(): Unit =
    assertEquals((1 :: Nil(), 2 :: 3 :: 4 :: Nil()), l.spanR(_ % 2 == 0))

  @Test def testReduce(): Unit =
    assertEquals(10, l.reduceR(_ + _))

  @Test def testReduceEmpty(): Unit =
    assertThrows(classOf[UnsupportedOperationException], () => Nil().reduceR((a,b) => a))

  @Test def testTakeRight(): Unit =
    assertEquals(2 :: 3 :: 4 :: Nil(), l.takeRightR(3))

  @Test def testTakeRightEmpty(): Unit =
    assertEquals(Nil(), Nil().takeRightR(3))
}
