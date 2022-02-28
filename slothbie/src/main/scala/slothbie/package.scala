import com.arkondata.slothql.cypher.CypherStatement.Prepared

package object slothbie extends FragmentsSyntax {

  case class PreparedPartially(parts: Seq[String], frs: Seq[SingleFragment[_]]) {

    def query[R]: Prepared[R] = Prepared[R](
      parts.mkString(""),
      Map.empty
    )
  }

  implicit class CypherStatementOps(val sc: StringContext) extends AnyVal {
    def cypher(frs: SingleFragment[_]*): PreparedPartially = PreparedPartially(sc.parts, frs)
  }
}
