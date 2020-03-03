package com.abraxas.slothql.newcypher.syntax

import com.abraxas.slothql.newcypher.CypherStatement

/** Advanced [[CypherSyntaxMainSpec]]
 *  - Referencing paths
 *  - Clauses WITH, UNWIND and OPTIONAL MATCH
 *  - {{{if}}} guards
 *  - Query UNION
 *  - Return: All, Nothing, Options
 */
class CypherSyntaxReadSpec extends CypherSyntaxBaseSpec {
  protected def cypherGen: CypherStatement.Gen = ???
}
