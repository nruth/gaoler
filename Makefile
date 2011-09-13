REBAR=rebar

.PHONY: all test docs

all: 
	$(REBAR) compile

test:
	$(REBAR) compile eunit

docs: 
	@echo "Not yet..."