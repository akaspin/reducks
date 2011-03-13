%%% @version 0.1

-module(reducks).
-export([snap/3, purge/2]).



snap(nocache, _, Make)->
    {ok, Data} = Make(),
    {ok, Data};

snap(connect, Spec, Make)->
    {ok, Client} = erldis:connect(),
    try
        {ok, Data} = snap(Client, Spec, Make),
        {ok, Data}
    after
        erldis:quit(Client)        
    end;

snap(Client, {Key, Field, Value}, {Make, Timeout}) ->
    case erldis:hget(Client, Key, Field) of
        Value ->
            {ok, found};
        _ ->
            snap(Client, Key, {Make, Timeout})
    end;
snap(Client, {Key, Field, Value}, Make) ->
    snap(Client, {Key, Field, Value}, {Make, 300000});
snap(Client, Key, {Make, Timeout}) ->
    %% Try get data
    case catch erldis:hgetall(Client, Key) of
        [] -> 
            % No data here - try lock
            KeyLock = <<Key/binary, ":lock">>,
            TS = get_timestamp(0),
            case catch erldis:setnx(Client, KeyLock, n_to_b(TS+Timeout)) of
                true ->
                    % Ok. Lock acquired
                    set_data(Client, Key, Make, Timeout, KeyLock);
                false -> 
                    % Lock is dirty. Let's do hard work.
                    LockTS = b_to_n(erldis:get(Client, KeyLock)),
                    
                    % Check expiration
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
                                    % We get lock!
                                    set_data(Client, Key, Make, 
                                             Timeout, KeyLock)
                            end
                    end;
                {'EXIT', _} ->
                    % Rare "unsubscribe" bug workaround.
                    % It appears in one out of ten of thousands of 
                    % "UNSUBSCRIBE" commands.
                    erldis:unsubscribe(Client),
                    snap(Client, Key, {Make, Timeout})
            end;
        {'EXIT', _} ->
            % See above
            erldis:unsubscribe(Client),
            snap(Client, Key, {Make, Timeout});
        Data -> 
            % all good - return data
            {ok, Data}
    end;
snap(Client, Key, Make) ->
    snap(Client, Key, {Make, 120000}).

%% @doc Purge keys by tags. 
purge(Client, Tags)->
    TagKeys = [<<T/binary, ":tag">> || T <- Tags],
    Keys = erldis:sunion(Client, [<<T/binary, ":tag">> || T <- Tags]),
    erldis:delkeys(Client, lists:append([Keys, TagKeys])).

%% Set data in cache
%% @private 
set_data(Client, Key, Make, Timeout, KeyLock) ->
    erldis:set_pipelining(Client, true),
    try Make() of
        {ok, Data} ->
            Fields = lists:filter(fun({K, _}) -> is_binary(K) end, Data),
            erldis:hmset(Client, Key, Fields),
            
            % Set expiration if needed
            case lists:keyfind(ttl, 1, Data) of
                {ttl, TTL} -> erldis:expire(Client, Key, TTL);
                false-> ok
            end,
            
            % Set tags if needed
            case lists:keyfind(tags, 1, Data) of
                {tags, Tags} -> 
                    [ erldis:sadd(Client, <<T/binary,":tag">>, Key) || 
                        T <- Tags ];
                false-> ok
            end,
            
            cleanup(Client, KeyLock),
            snap(Client, Key, {Make, Timeout})
    catch
        Err:Reason ->
            % Make  crashed
            cleanup(Client, KeyLock),
            error({Err, Reason})
    end.

%% @doc Cleanup
cleanup(Client, KeyLock)->
    erldis:publish(Client, KeyLock, <<"ok">>),
            erldis:del(Client, KeyLock),
            erldis:get_all_results(Client),
            erldis:set_pipelining(Client, false).

%% Convert integer to binary
%% @private 
n_to_b(N) ->
    list_to_binary(integer_to_list(N)).

%% @private 
b_to_n(nil) ->
    0;
%% @private 
b_to_n(B) ->
    list_to_integer(binary_to_list(B)).

%% @private 
get_timestamp(Diff) ->
    {Mega, Secs, Msecs} = now(),
    ((Mega*1000000 + Secs)*1000000 + Msecs) div 1000 + Diff.

%%
%% Tests
%%
-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


-endif.
