%%% @version 0.1

%%% @doc Dog-pile free operations for erldis. 
%%%
%%% I wrote this module for mochiweb-based web applications. Main problem 
%%% with concurrent caching is cache stampering aka "dog-pile" effect. 
%%%
%%% `reducks' is designed to deal with hash keys. It's well suitable 
%%% for caching web pages or json.


-module(reducks).
-export([snap/3, snap/4, purge/2, add_tags/3]).

%% Types
%% @type key() = binary(). Key in cache.
%% @type field_name() = binary(). Field name.
%% @type field_value() = binary(). Field value.
%% @type check_spec() = {field_name(), field_value()}. 
%%      Existing field check specification. Field name and value.
%%
%% @type data() = [{field(), value()}]. Data in cache.
%%
%% @type ttl() = integer() | infinity. Time to live in seconds.
%% @type make_fun() = ()-> {
%%          {data, data()},
%%          {ttl, ttl()}}. 
%%      Make data function. 
%% @type make_timeout() = integer(). Operation timeout.
%% @type make_spec() = {make_fun()} | {make_fun(), make_timeout()}. 
%%      Make data specification.

%% @spec snap(Client, Key::key(), Make::make_spec()) -> {ok, data()}
%% @doc Cache operation. Tryes to get data from cache. 
%% <ol>
%%  <li>If data not exists - tryes to aquire lock.</li>
%%  <li>If lock aquired - calls {@link make_fun(). make function} and 
%%      writes data to cache. </li>
%%  <li>If key locked by another process - waits for completion or for
%%      {@link make_timeout(). timeout} expiration.</li>
%%</ol>
snap(Client, Key, {Make}) ->
    snap(Client, Key, {Make, 120});
snap(Client, Key, {Make, Timeout}) ->
    try_get(Client, Key, Make, Timeout).

%% @spec snap(Client, Key::key(), Spec::check_spec(), Make::make_spec()) -> 
%%      {ok, data()} | {ok, equal}
%% @doc Cache operation with check value of existing key.
%% Acts like {@link snap/3.} but first check {@link check_spec(). field value} 
%% and if it equal to given - returns `{ok, equal}'. It's useful for "ETag".
snap(Client, Key, {Field, Value}, {Make}) ->
    snap(Client, Key, {Field, Value}, {Make, 120});
snap(Client, Key, {Field, Value}, {Make, Timeout}) ->
    %% Check equality of field
    case erldis:hget(Client, Key, Field) of
        Value -> 
            %% Ok. Return "304"
            {ok, equal};
        _ ->  
            %% Any other - try to process
            try_get(Client, Key, Make, Timeout)
    end.

%% @spec purge(Client, Key::key()) - any()
%% @doc Jently delete key after all served.
purge(Client, Key) ->
    KeyLock = get_lock_key(Key),
    case erldis:ttl(Client, KeyLock) of
        -1 ->
            %% no lock - delete
            erldis:del(Client, Key);
        LockTTL -> 
            %% lock exists - subscribe and wait
            Subscribers = erldis:subscribe(Client, KeyLock, self()),
            receive
                {message, KeyLock, <<"ok">>} ->
                    case erldis:unsubscribe(Client, KeyLock) of
                        {_, 0} -> 
                            erldis:del(Client, Key);
                        {_, _} -> 
                            purge(Client, Key)
                    end
            after LockTTL + Subscribers * 1000 ->
                    case erldis:unsubscribe(Client, KeyLock) of
                        {_, 0} -> 
                            erldis:del(Client, Key);
                        {_, _} -> 
                            purge(Client, Key)
                    end
            end
    end.

add_tags(_Client, _Key, []) ->
    ok;
add_tags(Client, Key, [Tag|Rest]) ->
    erldis:sadd(Client, <<"tag:", Tag/binary>>, Key),
    add_tags(Client, Key, Rest).



%% 
%% Private
%% 

%% @doc Try to get data 
try_get(Client, Key, Make, Timeout) ->
    %% Try get data
    case erldis:hgetall(Client, Key) of
        [] -> 
            %% No data here - check lock 
            try_set(Client, Key, Make, Timeout);
        Data -> 
            %% all good - return data
            {ok, Data}
    end.

%% @doc try to set data
try_set(Client, Key, Make, Timeout) ->
    KeyLock = get_lock_key(Key),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    timer:sleep(random:uniform(100)),
    case erldis:setnx(Client, KeyLock, <<"lock">>) of
        true ->
            %% ok - make it
            erldis:expire(Client, KeyLock, Timeout),
            {{data, Data}, {ttl, TTL}} = Make(),
            erldis:hmset(Client, Key, Data),
            % if TTL isn't infinity - set it
            if TTL =/= infinity -> 
                erldis:expire(Client, Key, TTL)
            end,
            erldis:publish(Client, KeyLock, <<"ok">>),
            erldis:del(Client, KeyLock),
            {ok, Data};
        false -> 
            %% can't aqquire lock
            wait(Client, Key, Make, Timeout)
    end.

%% wait for unlock or lock expiration
wait(Client, Key, Make, Timeout) ->
    KeyLock = get_lock_key(Key),
    case erldis:ttl(Client, KeyLock) of
        -1 ->
            try_get(Client, Key, Make, Timeout);
        LockTTL ->
            %% ohh let's wait
            Subscribers = erldis:subscribe(Client, KeyLock, self()),
            receive
                {message, KeyLock, <<"ok">>} ->
                    erldis:unsubscribe(Client, KeyLock),
                    try_get(Client, Key, Make, Timeout)
            after (LockTTL * 1000 - Subscribers * 1000) ->
                erldis:unsubscribe(Client, KeyLock),
                try_get(Client, Key, Make, Timeout)
            end
    end.


get_lock_key(Key) ->
    <<Key/binary, ":lock">>.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


-endif.
