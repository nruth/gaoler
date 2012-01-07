REBAR=rebar

.PHONY: all test docs deps

all: deps
	$(REBAR) compile

test: deps
	$(REBAR) compile eunit

deps: 
	mkdir .eunit
	cp src/gaoler.config.src .eunit/gaoler.config
	cp src/gaoler.config.src ebin/gaoler.config

clean:
	$(REBAR) clean
	rm test/*.beam

docs: 
	@echo "Not yet..."