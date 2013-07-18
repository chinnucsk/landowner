REBAR= rebar

.PHONY: all deps clean

all : deps
	$(REBAR) compile

deps: 
	$(REBAR) get-deps

clean:
	$(REBAR) clean
