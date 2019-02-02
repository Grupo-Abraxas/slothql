Cypher syntax
---------------------

Enable syntax extensions by importing `com.abraxas.slothql.cypher.syntax._`.

## Match

`Match` macro enables syntax for path patterns. It expects a partial function with a single `case` clause.
Patterns support both directions of arrows.
```scala
Match { case a -(b)> c -(d)> e <(f)- g => ... }
// MATCH (`a`) -[`b`]-> (`c`) -[`d`]-> (`e`) <-[`f`]- (`g`)
```

Matches can be optional. They can be nested.
```scala
Match { case a -(b)> c =>
  Match { case c -(d)> e =>
    Match.optional { case e <(f)- g =>
      ... }}}
// MATCH (`a`) -[`b`]-> (`c`)
// MATCH (`c`) -[`d`]-> (`e`)
// OPTIONAL MATCH (`e`) <-[`f`]- (`g`)
```


#### Pattern

The types of bind variables are either `Vertex` or `Edge`.
Patterns can define labels and property values with the corresponding keyword.
```scala
case (a@Vertex("x", "y")) -(b@Edge("x", "y"))> Vertex("z", "foo" := "bar") =>
// MATCH (`a`:`x`:`y`) -[`b`:`x`|`y`]-> (:`z`{ `foo`: \"bar\" })
```

As you could have seen the names of the variables are taken directly from the code
that can be seen as a bug or a feature.

Variable length patterns and path variables are supported.
```scala
case Vertex("A", "id" := 1) - es *:(4 - 10, Edge("foo", "bar")) > (b@Vertex("B")) =>
// Binding a variable length relationship pattern to a variable ('es') is deprecated and will be unsupported in a future version of neo4j
// MATCH (:`A`{ `id`: 1 }) -[`es`:`foo`|`bar`*4..10]-> (`b`:`B`) RETURN 1
```

```scala
case ps ::= (Vertex("A") - _ *:(0 - _, _) > Vertex("B")) =>
// MATCH `ps` = (:`A`) -[*0..]-> (:`B`)
```

Labels and properties can be non-literal values.
```scala
val labelA = "A"; val labelB = "B"; val labels = Set("Z", "W"); val types = Stream("foo", "bar"); val id = 100
case Vertex(`labelA`, `labelB`, "id" := `id`) - Edge(`types`) > (v@Vertex(`labels`)) =>
// MATCH (:`A`:`B`{ `id`: 100 }) -[:`foo`|`bar`]-> (`v`:`Z`:`W`)
```

Optional properties can be matched.
```scala
val idOpt = Option.empty[String]
case v@Vertex("id" :?= `idOpt`) =>
// MATCH (`v`)

val idOpt = Some("123")
case v@Vertex("id" :?= `idOpt`) =>
// MATCH (`v`{ `id`: \"123\" })
```

#### Where
Where clauses can be added to match with scala's `if` guards.
```scala
case v if v.id <= lit(1000L) =>
// MATCH (`v`) WHERE `id`(`v`) <= 1000
```

Condition in `if` guard can be an optional _known_ expression.
```scala
def condOpt(v: Vertex): Option[CypherFragment.Known[CypherFragment.Expr[Boolean]]] = None 
case v if condOpt(v) => 
// MATCH (`v`)

def condOpt(v: Vertex) = Some(v.prop[String]("name").matches(lit("foo.*bar")).known) 
case v if condOpt(v) => 
// MATCH (`v`) WHERE `v`.`name` =~ \"foo.*bar\"
```

## Expressions

| Expression                        | Syntax                    | Cypher                    | `Expr[?]`         |
| --------------------------------- | ------------------------- | ------------------------- | ----------------- |
| Insert literal value              | `lit(x)`                  | `x`                       | `X`
| Insert null value                 | `cypherNull[T]`           | `null`                    | `T`
| Call built-in function            | `'func.call[T](x, y)`     | `func(x, y)`              | `T`
| Conversion to string              | `x.asString`              | `toString(x)`             | `String`
| **Vertex & Edge** | | |
| Represent as map of properties    | `x.props`                 | `x`                       | `Map[String, Any]`
| Select property value             | `x.prop[T]("field")`      | `x.field`                 | `T`
| Select property value as Option   | `x.propOpt[T]("field")`   | `x.field`                 | `Option[T]`
| Built-in `id` function            | `x.id`                    | `id(x)`                   | `Long`
| Built-in `count` function         | `x.count`                 | `count(x)`                | `Long`
| Built-in `keys` function          | `x.keys`                  | `keys(x)`                 | `List[String]`
| **Vertex** | | |
| Built-in `labels` function        | `v.labels`                | `labels(v)`               | `List[String]`
| **Edge** | | |
| Built-in `type` function          | `e.tpe`                   | `type(e)`                 | `String`
| **Path** | | |
| Built-in `nodes` function         | `p.nodes`                 | `nodes(p)`                | `List[Vertex]`
| Built-in `relationships` function | `p.edges`                 | `relationships(p)`        | `List[Edge]`
| Built-in `length` function        | `p.length`                | `length(p)`               | `Long`
| **Rows** | | |
| Distinct rows                     | `distinct(x)`             | `DISTINCT x`              | `X`
| Collect rows to a list            | `collect(x)`              | `collect(x)`              | `List[X]`
| **List** | | |
| Build a list                      | `list(x0, x1, ...)`       | `[x0, x1, ...]`           | `List[X]`
| List comprehensions: map          | `xs.map(x => r)`                      | `[x in xs | r]`               | `List[R]`
| List comprehensions: filter       | `xs.filter(x => b)`                   | `[x in xs WHERE b]`           | `List[R]`
| List comprehensions: filter + map | `xs.withFilter(x => b).map(x => r)`   | `[x in xs WHERE b | r]`       | `List[R]`
| List reduction                    | `xs.reduce(r0)((x, r) => r1)`         | `reduce(r = r0, x IN xs | r1)`| `R`
| Lists concatenation               | `xs concat ys`, `xs ++ ys`| `xs + ys`                 | `List[X]`
| Index access                      | `xs at i`                 | `xs[i]`                   | `X`
| List slice                        | `xs.at(from, to)`         | `xs[from..to]`            | `List[X]`
| List slice                        | `xs.from(i)`              | `xs[i..]`                 | `List[X]`
| List slice                        | `xs.to(i)`                | `xs[..i]`                 | `List[X]`
| List length                       | `xs.size`                 | `size(xs)`                | `Long`
| Predicate: all                    | `xs.all(x => b)`          | `all(x IN xs WHERE b)`    | `Boolean`
| Predicate: any                    | `xs.any(x => b)`          | `any(x IN xs WHERE b)`    | `Boolean`
| ~~Predicate: exists~~             | `xs.exists(x => b)`       | `exists(x IN xs WHERE b)` | `Boolean`
| Predicate: none                   | `xs.none(x => b)`         | `none(x IN xs WHERE b)`   | `Boolean`
| Predicate: single                 | `xs.single(x => b)`       | `single(x IN xs WHERE b)` | `Boolean`
| **Map** | | |
| Build map from entries            | `dict("k" -> v, ...)`     | `{k: v, ...}`             | `T`
| Select map value                  | `m.value[T]("field")`     | `m.field`                 | `T`
| Add entries to a map              | `m.add("k" -> v), ...`    | `m.{.*, k: v, ...}`       | `Map[String, T]`
| **Case** | | |
| **Logic** | | |
| Negation                          | `!x`                      | `NOT x`           | Boolean
| Conjunction                       | `x and y`, `x && y`       | `x AND y`         | Boolean
| Disjunction                       | `x or y`, `x || y`        | `x OR y`          | Boolean
| Exclusive disjunction             | `x xor y`                 | `x XOR y`         | Boolean
| **Equality** | | |
| Equality                          | `x eq y`, `x === y`       | `x = y`           | Boolean
| Inequality                        | `x neq y`, `x <> y`       | `x <> y`          | Boolean
| Null equality                     | `x.isNull`                | `x IS NULL`       | Boolean
| Null inequality                   | `x.notNull`               | `x IS NOT NULL`   | Boolean
| **Comparison** | | |
| Less                              | `x lt y`, `x < y`         | `x < y`           | Boolean
| Less or equal                     | `x lte y`, `x <= y`       | `x <= y`          | Boolean
| Greater                           | `x gt y`, `x > y`         | `x > y`           | Boolean
| Greater or equal                  | `x gte y`, `x >= y`       | `x >= y`          | Boolean
| Contained in list                 | `x in list`               | `x IN list`       | Boolean
| **Numeric** | | |
| Negation                          | `-x`                      | `-x`              | Numeric
| Addition                          | `x + y`                   | `x + y`           | Numeric
| Subtraction                       | `x - y`                   | `x - y`           | Numeric
| Multiplication                    | `x * y`                   | `x * y`           | Numeric
| Division                          | `x / y`                   | `x / y`           | Numeric
| Modulo Division                   | `x % y`                   | `x % y`           | Numeric
| Exponentiation                    | `x ^ y`                   | `x ^ y`           | Numeric
| **String** | | |
| Substring test                    | `x contains y`            | `x CONTAINS y`    | Boolean
| Prefix test                       | `x startsWith y`          | `x STARTS WITH y` | Boolean
| Suffix test                       | `x endsWith y`            | `x ENDS WITH y`   | Boolean
| Regular expression match          | `x matches y`             | `x =~ y`          | Boolean
| Convert to lower case             | `x.toLower`               | `toLower(x)`      | String
| Convert to upper case             | `x.toUpper`               | `toUpper(x)`      | String
| String length                     | `x.size`                  | `size(x)`         | Long
| Convert to boolean                | `x.toBoolean`             | `toBoolean(x)`    | Boolean
| Convert to float                  | `x.toDouble`              | `toFloat(x)`      | Double
| Convert to integer                | `x.toLong`                | `toInteger(x)`    | Long
| Take leftmost characters          | `x takeLeft n`            | `left(x, n)`      | String
| Take rightmost characters         | `x takeRight n`           | `right(x, n)`     | String
| Replace string occurrences        | `x.replace(from, to)`     | `replace(x, from, to)`    | String
| Reverse string                    | `x.reverse`               | `reverse(x)`              | String
| Split string at given delimiter   | `x split delimiter`       | `split(x, delimiter)`     | List[String]
| Substring                         | `x substring start`       | `substring(x, start)`     | String
| Substring                         | `x.substring(start, len)` | `substring(x, start, len)`| String
| Surrounding whitespace removal    | `x.trim`                  | `trim(x)`         | String
| Leading whitespace removal        | `x.trimLeft`              | `lTrim(x)`        | String
| Trailing whitespace removal       | `x.trimRight`             | `rTrim(x)`        | String


#### Case
**Simple** case expressions allow to match a value by equality.
```scala
Match { case v =>
  v.labels whenUnsafe(
    lit("foo" :: Nil) -> lit(1),
    lit("bar" :: Nil) -> lit(2)
  )
}
// MATCH (`v`)
// RETURN
//  CASE `labels`(`v`)
//    WHEN [ \"foo\" ] THEN 1
//    WHEN [ \"bar\" ] THEN 2
//  END

Match { case v =>
  v.labels when(
    lit("foobar" :: Nil) -> lit(1)
  ) otherwise               lit(0)
}
// MATCH (`v`)
// RETURN
//  CASE `labels`(`v`)
//    WHEN [ \"foobar\" ] THEN 1
//    ELSE 0
//  END
```


**Generic** case expressions can match any condition.
```scala
Match { case v =>
  when(
    (lit("foo") in v.labels)    -> lit(1),
    (v.prop[Int]("x") % 2 <> 0) -> lit(2)
  ) otherwise                      lit(0)
}
// MATCH (`v`)
// RETURN
//  CASE
//    WHEN \"foo\" IN `labels`(`v`) THEN 1
//    WHEN `v`.`x` % 2 <> 0         THEN 2
//    ELSE 0
//  END
```
(it has `whenUnsafe` version without `otherwise` clause)


## Other Clauses

#### Unwind

#### With

#### Union


## Return

