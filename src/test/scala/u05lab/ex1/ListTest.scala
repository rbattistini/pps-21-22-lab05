package u05lab.ex1

import org.junit.Assert.assertThrows
import org.junit.Assert.assertEquals
import org.junit.Test
import u05lab.ex1.List.Nil

class ListTest {

  private val l = 1 :: 2 :: 3 :: 4 :: Nil()

  @Test def testZipRight(): Unit =
    assertEquals((1, 0) :: (2, 1) :: (3, 2) :: (4, 3) :: Nil(), l.zipRight)

  @Test def testZipRightEmpty(): Unit =
    assertEquals(Nil(), Nil().zipRight)

  @Test def testPartition(): Unit =
    assertEquals((2 :: 4 :: Nil(), 1 :: 3 :: Nil()), l.partition(_ % 2 == 0))

  @Test def testSpan(): Unit =
    assertEquals((2 :: 3 :: 4 :: Nil(), 1 :: Nil()), l.span(_ % 2 == 0))

  @Test def testReduce(): Unit =
    assertEquals(10, l.reduce(_ + _))

  @Test def testReduceEmpty(): Unit =
    assertThrows(classOf[UnsupportedOperationException], () => Nil().reduce((a,b) => a))

  @Test def testTakeRight(): Unit =
    assertEquals(2 :: 3 :: 4 :: Nil(), l.takeRight(3))

  @Test def testTakeRightEmpty(): Unit =
    assertEquals(Nil(), Nil().takeRight(3))

  @Test def testCollect(): Unit =
    assertEquals(2 :: 3 :: 4 :: Nil(), l.collect({case i if i < 4 => i + 1}))

}
