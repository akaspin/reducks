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

rebar:
	@-rm -rf $(BLD)
	@mkdir -p $(BLD)
	@cd $(BLD) && $(GIT) clone -q git://github.com/basho/rebar.git 
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
	@-rm -f TEST*
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean: rebar
	@-exec $(REBAR) clean
	@-rm -f erl_crash.dump
	@-rm -f TEST*
	
distclean: clean
	@-exec $(REBAR) delete-deps 
	@-rm -rf deps
	
run: compile
	@$(ERL) -pa $(DEPS) \
		-sname $(NAME) +K true +A 200

