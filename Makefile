# Build and ship the hydrogeochem Shiny image.
#
#   make archive   rebuild the image and write hydrogeochem-YYYYMMDD.tar.gz
#   make save      save the current image without rebuilding
#   make verify    list the *.rds files baked into the image
#   make deploy    archive, copy to the server, load and replace the container
#   make clean     remove local archives
#
# Docker needs root here, hence DOCKER defaults to "sudo docker". After adding
# yourself to the docker group, use: make DOCKER=docker archive

SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c

DOCKER   ?= sudo docker
IMAGE    ?= hydrogeochem:latest
NAME     ?= hydrogeochem
DATE     := $(shell date +%Y%m%d)
ARCHIVE  ?= $(NAME)-$(DATE).tar.gz

# Override on the command line, e.g.
#   make deploy SERVER=root@host SSH_PORT=22
SERVER   ?= root@example.com
SSH_PORT ?= 22
REMOTE   ?= ~/$(NAME).tar.gz

SAVE_CMD = $(DOCKER) save $(IMAGE) | gzip > $(ARCHIVE)

.PHONY: help build archive save verify deploy clean

help:
	@echo "make archive   rebuild $(IMAGE) and write $(ARCHIVE)"
	@echo "make save      save the current $(IMAGE) without rebuilding"
	@echo "make verify    list *.rds baked into $(IMAGE)"
	@echo "make deploy    archive, scp to SERVER, load and replace the container"
	@echo "make clean     remove local $(NAME)-*.tar.gz"
	@echo
	@echo "deploy overrides: SERVER=$(SERVER) SSH_PORT=$(SSH_PORT) REMOTE=$(REMOTE)"
	@echo "WARNING: deploy removes the old container, so runtime changes to"
	@echo "         chem_data.rds are lost unless backed up first."

build:
	$(DOCKER) build --network=host -t $(IMAGE) .

archive: build
	@echo "Saving $(IMAGE) -> $(ARCHIVE)"
	$(SAVE_CMD)
	@ls -lh $(ARCHIVE)

save:
	@echo "Saving $(IMAGE) -> $(ARCHIVE)"
	$(SAVE_CMD)
	@ls -lh $(ARCHIVE)

verify:
	$(DOCKER) run --rm $(IMAGE) \
		sh -c 'ls -l /srv/shiny-server/hydrogeochem/*.rds'

deploy: archive
	scp -P $(SSH_PORT) $(ARCHIVE) $(SERVER):$(REMOTE)
	ssh -p $(SSH_PORT) $(SERVER) '\
		set -e; \
		sudo docker load -i $(REMOTE); \
		sudo docker rm -f $(NAME) 2>/dev/null || true; \
		sudo docker run -d --name $(NAME) -p 8180:8180 --restart unless-stopped $(IMAGE)'

clean:
	rm -f $(NAME)-*.tar.gz
