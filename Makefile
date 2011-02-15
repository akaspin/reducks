GIT=`which git`
ERL=`which erl`
ERLC=`which erlc`
ESCRIPT=`which escript`
REBAR=./rebar

BLD=.build

NAME=reducks
DEPS=ebin deps/*/ebin

.PHONY: deps doc test

all: rebar deps compile

rebar:
	@-rm -rf $(BLD)
	@mkdir -p $(BLD)
	@cd $(BLD) && $(GIT) clone git://github.com/basho/rebar.git 
	@cd $(BLD)/rebar && exec make
	@-mv $(BLD)/rebar/rebar .
	@-rm -rf $(BLD)

deps: rebar
	@exec $(REBAR) get-deps
	@exec $(REBAR) update-deps

compile: rebar
	@exec $(REBAR) compile
	
doc: rebar
	@exec $(REBAR) doc skip_deps=true

test: compile 
	@-rm -rf .eunit
	@-rm TEST*
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean: rebar
	@exec $(REBAR) clean
	@-rm erl_crash.dump
	@-rm TEST*
	
distclean: rebar clean
	@exec $(REBAR) delete-deps && -rm -f rebar
	@-rm -rf $(BLD)
	
run: rebar compile
	@$(ERL) -pa $(DEPS) \
		-sname $(NAME) +K true +A 200

run1: rebar compile
	@$(ERL) -pa $(DEPS) \
		-sname reducks1 +K true +A 200