-module(redux_er_test).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


key_te_() -> fun() ->
    ?assertEqual(<<"key:lock">>, reducks:get_lock_key(<<"key">>))
    end.

normal_timeout_test_() -> 
    {"Timeout > make",
     setup, fun() -> 
                    er_pool:start_link(cache, "127.0.0.1", 6379, 25),
                    flushall()
     end,
     fun(_) -> 
             Renders = get_key(<<"renders">>),
             Gets = get_key(<<"gets">>),
             flushall(),
             ?assertEqual(<<"1">>, Renders),
             ?assertEqual(<<"500">>, Gets)
     end,
     {inorder, [
      {inparallel, make_tests(400, 120000, 1200)},
      fun() -> timer:sleep(100) end,
      {inparallel, make_tests(100, 120000, 1200)}
     ]}
     }.
small_timeout_test_() -> 
    {"Timeout = make",
     setup, fun flushall/0,
     fun(_) -> 
             Renders = get_key(<<"renders">>),
             Gets = get_key(<<"gets">>),
             flushall(),
             ?assertEqual(<<"1">>, Renders),
             ?assertEqual(<<"300">>, Gets)
     end,
     {inorder, [
      {inparallel, make_tests(300, 10, 1200)}
     ]}
     }.

race_op(_, Timeout, TTL) ->
    Key = <<"key">>,
    Data = [<<"one">>, <<"one">>],
    Make = fun() ->
                   timer:sleep(100),
                   incr(<<"renders">>),
                   {{data, Data}, {ttl, TTL}}
                   end,
    ?assertEqual({ok, Data}, reducks_er:snap(cache, Key, {Make, Timeout})),
    incr(<<"gets">>).

make_tests(N, Timeout, TTL) ->
    [fun() -> race_op(I, Timeout, TTL) end || I <- lists:seq(1, N) ].

flushall() ->
    ?assertEqual(ok, er:flushall(cache)).

incr(Key) ->
    er:incr(cache, Key).

get_key(Key) ->
    Res = er:get(cache, Key),
    Res.
    
    