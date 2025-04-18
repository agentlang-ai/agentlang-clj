.DEFAULT_GOAL := help
.PHONY: help

help: ## Print help for each target
	@grep '^[[:alnum:]_-]*:.* ##' $(MAKEFILE_LIST) \
	| sort | awk 'BEGIN {FS=":.* ## "}; {printf "%-25s %s\n", $$1, $$2};'

clean: ## Remove Postgres DB image
	./compose.sh clean
	#./postgres.sh clean

distclean: clean ## Remove Postgres DB image and local data
	./compose.sh distclean
	#./postgres.sh cleandata

build: ## Build Postgres DB image
	./compose.sh build
	#./postgres.sh build

rebuild: clean build ## Re-build Postgres DB image

run: build ## Run the Postgres DB image
	./compose.sh up
	#./postgres.sh run

up: ## Bring up Postgres container
	./compose.sh up

down: ## Bring down Postgres container
	./compose.sh down
