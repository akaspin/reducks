-module(massive_test).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

normal_timeout_test_() -> 
    {"Timeout > make",
     setup, fun test_util:flushall/0,
     fun(_) -> 
             catch_results(<<"1">>, <<"310">>),
             test_util:flushall()
     end,
     {inorder, [
      {inparallel, 
       [fun() -> race_op(I, 120000) end || I <- lists:seq(1, 300) ] },
      fun() -> timer:sleep(100) end,
      {inparallel, 
       [fun() -> race_op(I, 120000) end || I <- lists:seq(1, 10) ] }
     ]}
     }.
small_timeout_test_() -> 
    {"Timeout = make",
     setup, fun test_util:flushall/0,
     fun(_) -> 
             catch_results(<<"2">>, <<"300">>),
             test_util:flushall() 
     end,
      {inparallel, 
       [fun() -> race_op(I, 10) end || I <- lists:seq(1, 300) ] }
     }.

def_timeout_test_() -> 
    {"Default timeout",
     setup, fun test_util:flushall/0,
     fun(_) -> 
             catch_results(<<"1">>, <<"100">>),
             test_util:flushall()
     end,
     {inorder, [
      {inparallel, 
       [fun() -> def_timeout_op(I) end || I <- lists:seq(1, 100) ] }
     ]}
     }.

def_timeout_op(_) ->
    Key = <<"reducks-test:key">>,
    Data = [{<<"one">>, <<"one">>}],
    Make = make_make(Data),
    {ok, Client} = erldis:connect(),
    ?assertEqual({ok, Data}, 
                 reducks:snap(Client, Key, Make)),
    incr(<<"reducks-test:gets">>),
    erldis:quit(Client).

race_op(_, Timeout) ->
    Key = <<"reducks-test:key">>,
    Data = [{<<"one">>, <<"one">>}],
    Make = make_make(Data),
    {ok, Client} = erldis:connect(),
    ?assertEqual({ok, Data}, 
                 reducks:snap(Client, Key, {Make, Timeout})),
    incr(<<"reducks-test:gets">>),
    erldis:quit(Client).

catch_results(Renders, Gets) ->
    R = get_key(<<"reducks-test:renders">>),
    G = get_key(<<"reducks-test:gets">>),
    ?assertEqual(R, Renders),
    ?assertEqual(G, Gets).

make_make(Data) -> 
    fun() ->
        timer:sleep(100),
        incr(<<"reducks-test:renders">>),
        {ok, Data}
    end.

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
