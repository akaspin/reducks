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

deps: x
	@exec $(REBAR) get-deps
	@exec $(REBAR) update-deps

compile: x
	@exec $(REBAR) compile
	
doc: x
	@exec $(REBAR) doc skip_deps=true

test: compile
	@-rm -rf .eunit
	@-rm -f TEST*
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean: x
	@-exec $(REBAR) clean
	@-rm -f erl_crash.dump
	@-rm -f TEST*
	
distclean: clean
	@-exec $(REBAR) delete-deps 
	@-rm -rf deps
	
run: compile
	@$(ERL) -pa $(DEPS) \
		-sname $(NAME) +K true +A 200

x: rebar
	@-chmod +x rebar

rebar:
	@-rm -rf $(BLD)
	@mkdir -p $(BLD)
	@cd $(BLD) && $(GIT) clone -q git://github.com/basho/rebar.git 
	@cd $(BLD)/rebar && exec make
	@-mv $(BLD)/rebar/rebar .
	@-chmod +x rebar
	@-rm -rf $(BLD)
