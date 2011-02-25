-module(flow_test).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

persistence_test_() -> 
    {"Persistence",
     setup, fun test_util:flushall/0,
     fun(_) -> test_util:flushall() end,
     fun()-> 
             {ok, Client} = erldis:connect(),
             {Key, {Data, Data1}, {Make, Make1}} = get_assets(),
             
             ?assertEqual({ok, [Data]}, reducks:snap(Client, Key, {Make})),
             
             %% Data persist. New value can't replace old one
             ?assertNot({ok, [Data1]} == reducks:snap(Client, Key, {Make1})),
             ?assertEqual({ok, [Data]}, reducks:snap(Client, Key, {Make1})),
             
             erldis:delkeys(Client, [Key]),
             ?assertEqual({ok, [Data1]}, reducks:snap(Client, Key, {Make1})),
             erldis:quit(Client)
     end
     }.

equal_test_()->
    {"Equal",
     setup, fun test_util:flushall/0,
     fun(_) -> test_util:flushall() end,
     fun()-> 
             {ok, Client} = erldis:connect(),
             {Key, {Data, Data1}, {Make, Make1}} = get_assets(),
             
             ?assertEqual({ok, [Data]}, 
                          reducks:snap(Client, Key, Data, {Make})),
             ?assertEqual({ok, [Data]}, 
                          reducks:snap(Client, Key, Data1, {Make1})),
             ?assertEqual({ok, equal}, 
                          reducks:snap(Client, Key, Data, {Make})),
             erldis:quit(Client)
     end
     }.


get_assets() ->
    Key = <<"persistence/key">>,
    Data = {<<"field">>, <<"value1">>},
    Data1 = {<<"field">>, <<"value2">>},
    Make = make_maker([Data], 120000),
    Make1 = make_maker([Data1], 120000),
    {Key, {Data, Data1}, {Make, Make1}}.

make_maker(Data, TTL) ->
    fun() -> {{data, Data}, {ttl, TTL}} end.            

-endif.

