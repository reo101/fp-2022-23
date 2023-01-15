RM  ?= rm -f
ZIP ?= zip

SPEC  ?= CS
GROUP ?= 2
FN    ?= 62555

ZIP_NAME ?= spec$(SPEC)_group$(GROUP)_fn$(FN).zip

SRC_LIST ?= $(wildcard *.scm *.rkt)

.PHONY: archive
archive: $(ZIP_NAME)

$(ZIP_NAME): $(SRC_LIST)
	@$(ZIP) $@ $(SRC_LIST)

.PHONY: clean
clean:
	$(RM) $(ZIP_NAME)

.PHONY: love
love:
	@echo "not war"
