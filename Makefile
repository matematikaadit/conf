usage:
	@echo "make pull: pull the changes from configuration target"
	@echo "make push: push the changes to configuration target"
	@echo "make diff: diff the changes between configuration target and source"

pull:
	@bash sync.sh pull

push:
	@bash sync.sh push

diff:
	@bash sync.sh diff

.PHONY: usage pull push diff

