RM=`which rm`
GIT=`which git`
ERL=`which erl`
ERLC=`which erlc`
ESCRIPT=`which escript`
REBAR=./rebar

BLD=.build

NAME=reducks
DEPS=ebin deps/*/ebin

.PHONY: deps doc test

all: deps compile

deps:
	@exec $(REBAR) get-deps
	@exec $(REBAR) update-deps

compile:
	@exec $(REBAR) compile
	
doc:
	@exec $(REBAR) doc skip_deps=true

test: compile
	@-rm -rf .eunit
	@-rm -f TEST*
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@-exec $(REBAR) clean
	@-rm -f erl_crash.dump
	@-rm -f TEST*
	
# Rebuild rebar and clean
distclean: clean
	@-rm -rf $(BLD)
	@mkdir -p $(BLD)
	@cd $(BLD) && $(GIT) clone -q git://github.com/basho/rebar.git 
	@cd $(BLD)/rebar && exec make
	@-mv $(BLD)/rebar/rebar .
	@-rm -rf $(BLD)
	
	@-exec $(REBAR) delete-deps 
	@-rm -rf deps
	
run: compile
	@$(ERL) -pa $(DEPS) \
		-sname $(NAME) +K true +A 200

