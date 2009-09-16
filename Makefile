SOURCES := $(wildcard src/*.erl)
HEADERS := $(wildcard src/*.hrl)
MODULES := $(patsubst src/%.erl,%,$(SOURCES))
BEAMS := $(patsubst %,ebin/%.beam,$(MODULES))

all: $(BEAMS)


ebin/%.beam: src/%.erl $(HEADERS)
	erlc +debug_info  +warn_missing_spec -o ebin/ $<

dialyzer:
	dialyzer --src -r src/
