ifndef UID
	UID = $(shell id -u)
endif
export UID

ifndef GID
	GID = $(shell id -g)
endif
export GID

neo4j/plugins/apoc-4.1.0.0-all.jar:
	curl -L https://github.com/neo4j-contrib/neo4j-apoc-procedures/releases/download/4.1.0.0/apoc-4.1.0.0-all.jar -o neo4j/plugins/apoc-4.1.0.0-all.jar

start_dependencies: neo4j/plugins/apoc-4.1.0.0-all.jar
	docker-compose -f cicd/docker-compose.deps.yml --env-file ../.env up -d -V

stop_dependencies:
	docker-compose -f cicd/docker-compose.deps.yml down -v

sbt:
	docker-compose -f cicd/docker-compose.dev.yml run --rm sbt