import java.util
import java.util.UUID

import scala.collection.{ mutable, Iterable }
import scala.jdk.CollectionConverters.MapHasAsJava

import org.scalatest.{ EitherValues, OptionValues }
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import com.arkondata.slothql.cypher.CypherStatement.LiftValue
import com.arkondata.slothql.cypher.GraphElem

class BuildStatementsSpec extends AnyWordSpec with Matchers with OptionValues {
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

    "complex on create" in {
      val id       = UUID.randomUUID()
      val props    = Map("id" -> id)
      val lifter   = LiftValue[Map[String, UUID]]
      val prepared = cypher"MATCH (n:Node $props) return n".query[GraphElem.Node]

      prepared.template shouldBe "MATCH (n:Node $`param0`) return n"
      prepared.params.get("param0").value.toString shouldBe lifter.asParam(props).toString

    }
  }
}
