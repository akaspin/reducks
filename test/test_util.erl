-module(test_util).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

flushall() ->
    {ok, Client} = erldis:connect(),
    All = erldis:keys(Client, <<"reducks-test:*">>),
    erldis:delkeys(Client, All),
    erldis:quit(Client).

-endif.

