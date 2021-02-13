![Test](https://github.com/Grupo-Abraxas/slothql/workflows/Scala%20CI%20-%20Test/badge.svg)

#### Latest Snapshot / Pre-release
![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/Grupo-Abraxas/slothql?include_prereleases&label=pre-release)
![Scala CI - Publish Snapshot](https://github.com/Grupo-Abraxas/slothql/workflows/Scala%20CI%20-%20Publish%20Snapshot/badge.svg)
![Sonatype Nexus (Snapshots)](https://img.shields.io/nexus/s/com.arkondata/slothql-cypher_2.13?server=https%3A%2F%2Foss.sonatype.org)

#### Latest Release
![GitHub release (latest by date)](https://img.shields.io/github/v/release/Grupo-Abraxas/slothql)
![Scala CI - Publish Release](https://github.com/Grupo-Abraxas/slothql/workflows/Scala%20CI%20-%20Publish%20Release/badge.svg)
![Maven Central](https://img.shields.io/maven-central/v/com.arkondata/slothql-cypher_2.13)


# slothql
Graph Query Language for Scala

Under development.

Cypher
-----------------------------------------------------

[Cypher syntax](cypher/doc/syntax.md)

Contributing
-----------------------------------------------------
### Prerequisites:
  - Docker
  - Docker Compose

### Enable scalafmt hook

``` shell
git config core.hooksPath .git-hooks
```

### How to run test:

- Clone the project
- Start dependencies
``` shell
make start_dependencies
```
note: You can check healthy with `make ps`
- Run test
``` shell
make test
```
or yo can start sbt and test inside
``` shell
make sbt
```
- After all, stop dependencies
``` shell
make stop_dependencies
```


