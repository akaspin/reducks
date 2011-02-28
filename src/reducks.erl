%%% @version 0.1

-module(reducks).
-export([snap/3, snap/4, 
         mark/3, purge/2, marked/2]).


-type key()::binary().
-type field()::binary().
-type value()::binary().
-type time_out()::integer().
-type hash():: [{field(), value()}].

-type client()::pid().

-type make_fun()::fun(()-> 
        {{data, hash()}, {ttl, integer()|infinity}} | any()).
-type make_spec():: {make_fun()} | {make_fun(), time_out()}.

%% Types 
%% @type key() = binary(). Key. 
%% @type field() = binary(). Field name in hashset.
%% @type value() = binary(). Field value in hashset.
%% @type time_out() = integer(). Timeout in miliseconds.
%% @type hash() = [{field(), value()}]. Record in hashset.
%% @type client() = pid(). Erldis client.
%% @type make_fun() = ()->{{data, hash()}, {ttl, integer()|infinity}} | any().
%%          Make function. 
%% @type make_spec() = {make_fun()} | {make_fun(), time_out()}. Make 
%%          specification. Make function with timeout.

%% @doc Try to retrieve data.
%% @spec snap(Client::client(), Key::key(), Make::make_spec())-> 
%%           {ok, hash()} | {error, any()}
-spec snap(Client::client(), Key::key(), Make::make_spec())-> 
          {ok, hash()} | {error, any()}.
snap(Client, Key, {Make}) ->
    snap(Client, Key, {Make, 120000});
snap(Client, Key, {Make, Timeout}) ->
    %% Try get data
    case catch erldis:hgetall(Client, Key) of
        [] -> 
            %% No data here - try lock
            KeyLock = <<Key/binary, ":lock">>,
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
                    %% Rare "unsubscribe" bug workaround.
                    %% It appears in one out of ten of thousands of 
                    %% "UNSUBSCRIBE" commands.
                    erldis:unsubscribe(Client),
                    snap(Client, Key, {Make, Timeout})
            end;
        {'EXIT', _} ->
            %% See above
            erldis:unsubscribe(Client),
            snap(Client, Key, {Make, Timeout});
        Data -> 
            %% all good - return data
            {ok, Data}
    end.

%% @doc Try to retrieve data with pre-test.
%% @spec snap(Client::client(), Key::key(), CheckSpec::{field(), value()}, 
%%          Make::make_spec())-> {ok, equal} | {ok, hash()} | {error, any()}
-spec snap(Client::client(), Key::key(), CheckSpec::{field(), value()}, 
           Make::make_spec())->  {ok, equal} | {ok, hash()} | {error, any()}.
snap(Client, Key, {Field, Value}, {Make}) ->
    snap(Client, Key, {Field, Value}, {Make, 300000});

snap(Client, Key, {Field, Value}, {Make, Timeout}) ->
    case erldis:hget(Client, Key, Field) of
        Value ->
            {ok, equal};
        _ ->
            snap(Client, Key, {Make, Timeout})
    end.


%% @private 
set_data(Client, Key, Make, Timeout, KeyLock) ->
    try
        erldis:set_pipelining(Client, true),
        Data = Make(),
        Fields = lists:filter(fun({K, _}) -> is_binary(K) end, Data),
        
        case lists:keyfind(ttl, 1, Data) of
            %% Set expiration if needed
            {ttl, TTL} -> erldis:expire(Client, Key, TTL);
            false-> ok
        end,
        
        erldis:hmset(Client, Key, Fields),
        cleanup(Client, KeyLock),
        snap(Client, Key, {Make, Timeout})
    catch
        Err:Reason ->
            cleanup(Client, KeyLock),
            error({Err, Reason})
    end.

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

%% @doc Mark keys with tags.
mark(Client, Keys, Tags)->
    erldis:set_pipelining(Client, true),
    [ erldis:sadd(Client, Tag, Key) || Key <- Keys, Tag <- make_tags(Tags) ],
    erldis:get_all_results(Client),
    erldis:set_pipelining(Client, false).

%% @doc Get keys by tags.
marked(Client, Tags) ->
    erldis:sunion(Client, make_tags(Tags)).

%% @doc Purge keys
purge(Client, Tags) ->
    Keys = marked(Client, Tags),
    erldis:delkeys(Client, lists:flatten([make_tags(Tags), Keys])).

%% @private
make_tags(Tags)->
    [ <<Tag/binary, ":tag">> || Tag <-Tags ].

%%
%% Tests
%%
-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


-endif.
