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

start_dependencies:
	$(COMPOSE_CMD) -f cicd/docker-compose.deps.yml --env-file ../.env up -d -V

stop_dependencies:
	$(COMPOSE_CMD) -f cicd/docker-compose.deps.yml down -v


ps:
	$(COMPOSE_CMD) -f cicd/docker-compose.deps.yml --env-file ../.env ps

logs:
	$(COMPOSE_CMD) -f cicd/docker-compose.deps.yml --env-file ../.env logs -f

sbt:
	$(COMPOSE_CMD) -f cicd/docker-compose.dev.yml run --rm sbt

test:
	$(COMPOSE_CMD) -f cicd/docker-compose.dev.yml run --rm sbt test