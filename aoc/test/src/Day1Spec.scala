import zio.*
import zio.test.*

object Day1Spec extends ZIOSpecDefault {
  def spec = suite("Day1Spec")(
    test("example test") {
      assertTrue(1 + 1 == 2)
    }
  )
}
