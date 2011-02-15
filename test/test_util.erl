-module(test_util).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").



flushall() ->
    {ok, Client} = erldis:connect(),
    ?assertEqual(ok, erldis:flushall(Client)),
    erldis:quit(Client).

-endif.

