
### Example 1

```scala
  ScalaExpr[Book].reviews.map(_.choose(
    _.on[UserReview].split(_.user, _.text, _.vote),
    _.on[AnonReview].text,
    _.on[CustomReview].choose(
      _.on[Reviews].reviews.map(_.choose(
        _.on[AnonReview].text
      )),
      _.on[ImageReviewAttachment].split(_.review.split(_.user, _.text), _.url)
    )
  ))
```

![Example1](examples/example-1.png)


### Example 2

```scala
  ScalaExpr[Review].choose(
    _.on[UserReview].split(_.user, _.text, _.vote),
    _.on[AnonReview].text
  )
```

![Example2](examples/example-2.png)

### Example 3

```scala
ScalaExpr[Book].split(_.title, _.author.map(_.name), _.meta.isbn)
```

![Example3](examples/example-3.png)

### Example 4

```scala
ScalaExpr[Book].split(
  _.meta.isbn,
  _.self
)
```

![Example4](examples/example-4.png)

### Example 5

```scala
ScalaExpr[Book].meta.isbn.split(_.const(0))
```

![Example5](examples/example-5.png)
