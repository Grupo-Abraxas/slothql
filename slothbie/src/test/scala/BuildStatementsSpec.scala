import java.util.UUID

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import com.arkondata.slothql.cypher.GraphElem

class BuildStatementsSpec extends AnyWordSpec with Matchers {
  import slothbie._

  "BuildStatement" should {
    "simple spec" in {
      val prepared = cypher"MATCH (n: Node) return n".query[GraphElem.Node]
      prepared.template shouldBe "MATCH (n: Node) return n"
    }

    "pass primitive params" in {
      val id       = UUID.randomUUID()
      val prepared = cypher"MATCH (n:Node {id: $id}) return n".query[GraphElem.Node]

      prepared.template shouldBe "MATCH (n:Node {id: $`param0`}) return n"
      prepared.params shouldBe Map("param0" -> id)

    }
  }
}
