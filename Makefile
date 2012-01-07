REBAR=rebar

.PHONY: all test docs deps

all: deps
	$(REBAR) compile

test: deps
	$(REBAR) compile eunit

ct: deps
	$(REBAR) compile ct

deps: 
	mkdir -p .eunit
	cp src/gaoler.config.src .eunit/gaoler.config
	cp src/gaoler.config.src ebin/gaoler.config

clean:
	$(REBAR) clean
	rm test/*.beam
	rm ebin/gaoler.config
	rm .eunit/gaoler.config

docs: 
	@echo "Not yet..."