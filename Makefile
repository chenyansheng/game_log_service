SHELL := /bin/bash
.PHONY: all clean
ERL := erl
EBIN_DIR := ebin

all: apps
	@$(ERL) -pa $(EBIN_DIR) -noinput \
		-eval "case make:all() of error -> halt(1); _ -> halt(0) end"

apps:
	(mkdir -p $(EBIN_DIR))
	(cp -rf src/*.app $(EBIN_DIR))

clean:
	@echo "clean..."
	(rm -rf ebin/*; rm -rf *.dump; rm -rf log/*.log; rm -rf game_log)
