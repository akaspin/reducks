-module(redux_race_test).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


key_test_() -> fun() ->
    ?assertEqual(<<"key:lock">>, reducks:get_lock_key(<<"key">>))
    end.

normal_timeout_test_() -> 
    {"Timeout > make",
     setup, fun flushall/0,
     fun(_) -> 
             Renders = get_key(<<"renders">>),
             Gets = get_key(<<"gets">>),
             flushall(),
             ?assertEqual(<<"1">>, Renders),
             ?assertEqual(<<"400">>, Gets)
     end,
     {inorder, [
      make_tests(300, 120000, 1200),
      fun() -> timer:sleep(100) end,
      make_tests(100, 120000, 1200)
     ]}
     }.
small_timeout_test_() -> 
    {"Timeout = make",
     setup, fun flushall/0,
     fun(_) -> 
             Renders = get_key(<<"renders">>),
             Gets = get_key(<<"gets">>),
             flushall(),
             ?assertEqual(<<"2">>, Renders),
             ?assertEqual(<<"400">>, Gets)
     end,
     {inorder, [
      make_tests(300, 10, 1200),
      fun() -> timer:sleep(100) end,
      make_tests(100, 10, 1200)
     ]}
     }.

race_op(_, Timeout, TTL) ->
    Key = <<"key">>,
    Data = [{<<"one">>, <<"one">>}],
    Make = fun() ->
                   timer:sleep(100),
                   incr(<<"renders">>),
                   {{data, Data}, {ttl, TTL}}
                   end,
    {ok, Client} = erldis:connect(),
    Snapped = reducks:snap(Client, Key, {Make, Timeout}),
    ?assertEqual({ok, Data}, Snapped),
    incr(<<"gets">>),
    erldis:quit(Client).

make_tests(N, Timeout, TTL) ->
    {inparallel, 
        [fun() -> race_op(I, Timeout, TTL) end || I <- lists:seq(1, N) ]
     }.

flushall() ->
    {ok, Client} = erldis:connect(),
    ?assertEqual(ok, erldis:flushall(Client)),
    erldis:quit(Client).

incr(Key) ->
    {ok, Client} = erldis:connect(),
    erldis:incr(Client, Key),
    erldis:quit(Client).

get_key(Key) ->
    {ok, Client} = erldis:connect(),
    Res = erldis:get(Client, Key),
    erldis:quit(Client),
    Res.
    
-endif.
