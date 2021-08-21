package mlang

import org.junit.Test
import org.junit.Assert._
import scala.language.implicitConversions

import cats.data.State


case class Robot(
  id: Long
)
final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
}


val nextLong: State[Seed, Long] = State(seed => (seed.next, seed.long))

val createRobot: State[Seed, Robot] = for {
  id <- nextLong,
} yield Robot(id)

class BaseTests {
  @Test def test(): Unit = {
  }
}