package com.abraxas.slothql.newcypher.syntax

import com.abraxas.slothql.newcypher.CypherStatement

/** Advanced pattern matching.
 *  - Node: labels, props
 *  - Rel: labels, props, variable length paths
 *  - Matching paths with many elements (macro test)
 */
class CypherSyntaxPatternSpec extends CypherSyntaxBaseSpec {
  protected def cypherGen: CypherStatement.Gen = ???
}
