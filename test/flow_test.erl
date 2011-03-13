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
             
             ?assertEqual({ok, [Data]}, reducks:snap(Client, Key, Make)),
             
             %% Data persist. New value can't replace old one
             ?assertNot({ok, [Data1]} == reducks:snap(Client, Key, Make1)),
             ?assertEqual({ok, [Data]}, reducks:snap(Client, Key, Make1)),
             
             erldis:delkeys(Client, [Key]),
             ?assertEqual({ok, [Data1]}, reducks:snap(Client, Key, Make1)),
             erldis:quit(Client)
     end}.

found_test_()->
    {"Equal",
     setup, fun test_util:flushall/0,
     fun(_) -> test_util:flushall() end,
     fun()-> 
             {ok, Client} = erldis:connect(),
             io:format(user, ">~p~n", [{{D1F, D1V}, {D2F, D2V}}]),
             
             ?assertEqual({ok, [{D1F, D1V}]}, 
                          reducks:snap(Client, {Key, D1F, D1V}, Make)),
             ?assertEqual({ok, [{D1F, D1V}]}, 
                          reducks:snap(Client, {Key, D2F, D2V}, Make1)),
             ?assertEqual({ok, found}, 
                          reducks:snap(Client, {Key, D1F, D1V}, Make)),
             erldis:quit(Client)
     end}.

crash_test_() ->
    {"Crash in make fun",
     setup, fun test_util:flushall/0,
     fun(_) -> test_util:flushall() end,
     fun()-> 
             {ok, Client} = erldis:connect(),
             {Key, {_, _}, {_, _}} = get_assets(),
             MakeCrash = fun() -> throw(test) end,
             
             ?assertException(error, {throw, test}, 
                              reducks:snap(Client, Key, MakeCrash)),
             erldis:quit(Client)
     end}.

tags_test_() ->
    {"Tags",
     setup, fun test_util:flushall/0,
     fun(_) -> test_util:flushall() end,
     fun()-> 
             {ok, Client} = erldis:connect(),
             Key1 = <<"reducks-test:tag/one">>,
             Key2 = <<"reducks-test:tag/two">>,
             Key3 = <<"reducks-test:tag/three">>,
             Tag1 = <<"reducks-test:tag/1">>,
             Tag2 = <<"reducks-test:tag/2">>,
             
             reducks:snap(Client, Key1, 
                          fun()-> 
                                  {ok, [{<<"f">>, <<"v">>}, {tags, [Tag1]}]} 
                          end),
             reducks:snap(Client, Key2, 
                          fun()-> 
                                   {ok, [{<<"f">>, <<"v">>}, {tags, [Tag2]}]} 
                           end),
             reducks:snap(Client, Key3, 
                          fun()-> {ok, [{<<"f">>, <<"v">>}, 
                                         {tags, [Tag1, Tag2]}]} 
                           end),
             
             reducks:purge(Client, [Tag1]),
             
             ?assertNot(erldis:exists(Client,<<"reducks-test:tag/one">>)),
             ?assert(erldis:exists(Client,<<"reducks-test:tag/two">>)),
             ?assertNot(erldis:exists(Client,<<"reducks-test:tag/three">>)),
             ?assertNot(erldis:exists(Client,<<"reducks-test:tag/1:tag">>)),
             
             reducks:purge(Client, [Tag2]),

             ?assertNot(erldis:exists(Client,<<"reducks-test:tag/two">>)),
             ?assertNot(erldis:exists(Client,<<"reducks-test:tag/three">>)),
             ?assertNot(erldis:exists(Client,<<"reducks-test:tag/2:tag">>)),
             
             erldis:quit(Client)
     end}.


get_assets() ->
    Key = <<"reducks-test:persistence/key">>,
    Data = {<<"field">>, <<"value1">>},
    Data1 = {<<"field">>, <<"value2">>},
    Make = make_maker([Data], 120000),
    Make1 = make_maker([Data1], 120000),
    {Key, {Data, Data1}, {Make, Make1}}.

make_maker(Data, TTL) ->
    fun() -> {ok, Data ++ [{ttl, TTL}]} end.            

-endif.

