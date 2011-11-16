REBAR=rebar

.PHONY: all test docs

all: 
	$(REBAR) compile

test:
	$(REBAR) compile eunit

clean:
	$(REBAR) clean
	rm test/*.beam

docs: 
	@echo "Not yet..."