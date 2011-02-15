%%% @version 0.1

-module(reducks).
-export([snap/3, snap/4, purge/2]).

snap(Client, Key, {Field, Value}, {Make}) ->
    snap(Client, Key, {Field, Value}, {Make, 120000});
snap(Client, Key, {Field, Value}, {Make, Timeout}) ->
    case erldis:hget(Client, Key, Field) of
        Value ->
            {ok, equal};
        _ ->
            snap(Client, Key, {Make, Timeout})
    end.

snap(Client, Key, {Make}) ->
    snap(Client, Key, {Make, 120000});
snap(Client, Key, {Make, Timeout}) ->
    %% Try get data
    case catch erldis:hgetall(Client, Key) of
        [] -> 
            %% No data here - try lock
            KeyLock = get_lock_key(Key),
            TS = get_timestamp(0),
            case catch erldis:setnx(Client, KeyLock, n_to_b(TS+Timeout)) of
                true ->
                    %% Ok. Lock acquired
                    set_data(Client, Key, Make, Timeout, KeyLock);
                false -> 
                    % Lock is dirty. Let's do hard work.
                    LockTS = b_to_n(erldis:get(Client, KeyLock)),
                    
                    %% Check expiration
                    case LockTS  > TS of
                        true ->
                            Subs = erldis:subscribe(Client, KeyLock, self()),
                            receive
                                {message, KeyLock, _} ->
                                    erldis:unsubscribe(Client),
                                    snap(Client, Key, {Make, Timeout})
                            after LockTS - TS + Subs * Timeout ->
                                    
                                    erldis:unsubscribe(Client),
                                    snap(Client, Key, {Make, Timeout})
                            end;
                        false -> 
                            GSTS = erldis:getset(Client, KeyLock, 
                                          n_to_b(TS+Timeout+1000 )),
                            case b_to_n(GSTS) > TS of
                                true -> 
                                    snap(Client, Key, {Make, Timeout});
                                false -> 
                                    %% We get lock!
                                    set_data(Client, Key, Make, 
                                             Timeout, KeyLock)
                            end
                    end;
                {'EXIT', _} ->
                    %% Rare bug workaround
                    erldis:unsubscribe(Client),
                    snap(Client, Key, {Make, Timeout})
            end;
        {'EXIT', _} ->
            %% Rare bug workaround
            erldis:unsubscribe(Client),
            snap(Client, Key, {Make, Timeout});
        Data -> 
            %% all good - return data
            {ok, Data}
    end.

%% @doc Purge keys
purge(Client, Keys) ->
    erldis:delkeys(Client, Keys).

%% 
%% Private 
%% 

set_data(Client, Key, Make, Timeout, KeyLock) ->
    {{data, Data}, {ttl, TTL}} = Make(),
    erldis:hmset(Client, Key, Data),
    if TTL =/= infinity ->
           %% Set expiration if needed
           erldis:expire(Client, Key, TTL)
    end,
    erldis:publish(Client, KeyLock, <<"ok">>),
    erldis:del(Client, KeyLock),
    snap(Client, Key, {Make, Timeout}).

%% Convert integer to binary
n_to_b(N) ->
    list_to_binary(integer_to_list(N)).

b_to_n(nil) ->
    0;
b_to_n(B) ->
    list_to_integer(binary_to_list(B)).

get_timestamp(Diff) ->
    {Mega, Secs, Msecs} = now(),
    ((Mega*1000000 + Secs)*1000000 + Msecs) div 1000 + Diff.

%% @spec get_loget_lock_key(Key::string()) -> string()
%% @doc Get lock key
get_lock_key(Key) ->
    <<Key/binary, ":lock">>.

%%
%% Tests
%%
-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


-endif.
