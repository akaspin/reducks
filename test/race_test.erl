-module(race_test).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

race_test_() ->
    {setup, fun() -> clean() end,
     fun(_)-> clean() end,
     {inparallel, 
      [
       simple_op(),
       simple_op(),
       simple_op(),
       simple_op(),
       simple_op(),
       simple_op()
       
      ]}
     }.

%% --------

simple_op() -> fun() ->
    Key = <<"key">>,
    D = "Some Data",
    Make = fun() -> 
        timer:sleep(2000),
        io:format(user, "Render~n", []),
        {{data, [{<<"f">>, list_to_binary(D)}]}, {ttl, 300}}
    end,
    Expect = {ok, [{<<"f">>, list_to_binary(D)}]},
    {ok, Client} = erldis:connect(),
    reducks:snap(Client, Key, {Make}),
    erldis:quit(Client)
    end.

clean() ->
    {ok, Client} = erldis:connect(),
    ?assertEqual(ok, erldis:flushdb(Client)),
    erldis:quit(Client).

%% -endif.
