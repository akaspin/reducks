
-module(reducks_er).
-export([snap/3]).

snap(Client, Key, {Make}) ->
    snap(Client, Key, {Make, 120000});
snap(Client, Key, {Make, Timeout}) ->
    try_get(Client, Key, Make, Timeout).

%% 
%% Private
%% 

try_get(Client, Key, Make, Timeout) ->
    KeyLock = get_lock_key(Key),
    
    %% Try get data
    case catch er:hgetall(Client, Key) of
        [] -> 
            %% No data here - try lock
            TS = get_timestamp(0),
            case catch er:setnx(Client, KeyLock, n_to_b(TS+Timeout+1)) of
                true ->
                    %% Ok. Lock acquired
                    set_data(Client, Key, Make, Timeout, KeyLock, TS);
                false -> 
                    % Lock is dirty. Let's do hard work.
                    % subscribe to lock
                    LockTS = b_to_n(er:get(Client, KeyLock)),
                    
                    %% Check expiration
                    case LockTS+Timeout > TS of
                        true ->
                            {Sub, _} = er:subscribe(Client, KeyLock),
                            _ = er:er_next(Sub),
                            er:unsubscribe(Client, KeyLock),
                            Sub ! shutdown,
                            try_get(Client, Key, Make, Timeout);
                        false -> 
                            %% We get lock!
                            er:getset(Client, KeyLock, 
                                          n_to_b(TS+Timeout+1)),
                            set_data(Client, Key, Make, Timeout, KeyLock, TS)
                    end;
                {'EXIT', _} ->
                    er:unsubscribe(Client, KeyLock),
                    try_get(Client, Key, Make, Timeout)
            end;
        {'EXIT', _} ->
            er:unsubscribe(Client, KeyLock),
            try_get(Client, Key, Make, Timeout);
            
        Data ->
            %% all good - return data
            {ok, Data}
    end.

set_data(Client, Key, Make, Timeout, KeyLock, TS) ->
    case er:hgetall(Client, Key) of
        [] -> 
            {{data, Data}, {ttl, TTL}} = Make(),
            er:hmset(Client, Key, Data),
            if TTL =/= infinity ->
                   %% Set expiration if needed
                   er:expire(Client, Key, TTL)
            end,
            er:publish(Client, KeyLock, <<"ok">>),
            er:del(Client, KeyLock),
            
            try_get(Client, Key, Make, Timeout);
        Data -> 
            %% all good - return data
            {ok, Data}
    end.

n_to_b(N) ->
    list_to_binary(integer_to_list(N)).

b_to_n({error, _}) ->
    0;
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
