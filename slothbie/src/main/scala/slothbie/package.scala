import com.arkondata.slothql.cypher.CypherStatement.Prepared

package object slothbie extends FragmentsSyntax {

  case class PreparedPartially(parts: Seq[String], frs: Seq[SingleFragment[_]]) {

    def query[R]: Prepared[R] = {
      val intercalations = frs.zipWithIndex.map { case (f, i) => (f, s"param$i") }
      Prepared[R](
        parts.zipAll(intercalations.map(v => s"$$`${v._2}`"), "", "").map { case (c0, c1) => s"$c0$c1" }.mkString(""),
        intercalations.map { case (v, k) =>
          (k, v.toParam)
        }.toMap
      )
    }
  }

  implicit class CypherStatementOps(val sc: StringContext) extends AnyVal {
    def cypher(frs: SingleFragment[_]*): PreparedPartially = PreparedPartially(sc.parts, frs)
  }
}
