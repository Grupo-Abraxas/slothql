package com.abraxas.slothql.newcypher.syntax

import com.abraxas.slothql.newcypher.CypherStatement

/** Simplest spec: match and return.
 *  - Basic matching
 *  - Basic expressions
 *  - Returning: expr (with `as`), tuple
 *  - Nesting matches
 */
class CypherSyntaxMainSpec extends CypherSyntaxBaseSpec {
  protected def cypherGen: CypherStatement.Gen = ???
}
