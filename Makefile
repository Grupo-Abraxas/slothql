ifndef BRANCH_NAME
	BRANCH_NAME = $(shell git rev-parse --abbrev-ref HEAD)
endif

ifndef UID
	UID = $(shell id -u)
endif
export UID

ifndef GID
	GID = $(shell id -g)
endif
export GID

COMPOSE_CMD = docker-compose -p $(BRANCH_NAME)

neo4j/plugins/apoc-4.1.0.0-all.jar:
	curl -L https://github.com/neo4j-contrib/neo4j-apoc-procedures/releases/download/4.1.0.0/apoc-4.1.0.0-all.jar -o neo4j/plugins/apoc-4.1.0.0-all.jar

start_dependencies: neo4j/plugins/apoc-4.1.0.0-all.jar
	$(COMPOSE_CMD) -f cicd/docker-compose.deps.yml --env-file ../.env up -d -V

stop_dependencies:
	$(COMPOSE_CMD) -f cicd/docker-compose.deps.yml down -v


ps: neo4j/plugins/apoc-4.1.0.0-all.jar
	$(COMPOSE_CMD) -f cicd/docker-compose.deps.yml --env-file ../.env ps

logs: neo4j/plugins/apoc-4.1.0.0-all.jar
	$(COMPOSE_CMD) -f cicd/docker-compose.deps.yml --env-file ../.env logs -f

sbt:
	$(COMPOSE_CMD) -f cicd/docker-compose.dev.yml run --rm sbt

test:
	$(COMPOSE_CMD) -f cicd/docker-compose.dev.yml run --rm sbt test