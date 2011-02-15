-module(basic_test).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


key_test_() -> fun() ->
    ?assertEqual(<<"key:lock">>, reducks:get_lock_key(<<"key">>))
    end.

-endif.