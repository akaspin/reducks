-module(tags_test).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

tag_test_()->
    {"Tags",
     setup, fun test_util:flushall/0,
     fun(_) -> test_util:flushall() end,
     fun()-> 
             Tag1 = <<"one">>,
             Tag2 = <<"two">>,
             Keys1 = [<<"key1-one">>, <<"key1-two">>],
             Keys2 = [<<"key2-one">>, <<"key2-two">>],
             {ok, Client} = erldis:connect(),
             ?assertEqual(ok, reducks:mark(Client, Keys1, [Tag1])),
             ?assertEqual(ok, reducks:mark(Client, Keys2, [Tag1, Tag2])),
             ?assertEqual([], lists:subtract(
                            lists:flatten(Keys1, Keys2), 
                            reducks:marked(Client, [Tag1, Tag2]))),
             ?assertEqual([], lists:subtract(
                            lists:flatten(Keys1, Keys2), 
                            reducks:marked(Client, [Tag1]))),
             ?assertEqual([], lists:subtract(Keys2, 
                            reducks:marked(Client, [Tag2]))),
             ?assertEqual([], reducks:marked(Client, [<<"noexist">>])),
             
             reducks:purge(Client, [Tag1, Tag2]),
             ?assertEqual([], reducks:marked(Client, [Tag1, Tag2])),
             
             erldis:quit(Client)
     end
     }.





-endif.