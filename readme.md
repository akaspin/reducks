# reducks

*reducks* is tool for working with [Redis](http://redis.io/) hashsets in 
concurrent manner.

I'm using *reducks* for my web-applications and internal projects.

## Huh?

Imagine a high load cluster of web-servers. Multiple simultaneous 
connections. And often many parallel processes need to access the same data. 
Ok, let's cache it. However, the processes are executed in *parallel*. And the 
data may be missing in the cache to the start of each of them. Welcome to hell 
of cache stampeding aka "Dog-pile effect". Each process produces the data: 
asks the database (and of course loading it), load files from disk et cetera. 
And stores data to cache. Again and again. Well, you understand me.

*reducks* dealt with a dog pile by forcing all processes to wait until one 
of them does not put data into the cache. Regardless of whether these 
processes on one or several unrelated nodes. This is the basic idea.

## Build and testing

*reducks* uses [rebar](https://github.com/basho/rebar) for build. But there 
are also the usual `Makefile`.

    git clone https://github.com/akaspin/reducks.git
    cd reducks
    make
    
You can test *reducks* by

    make test
    
If you are using [rebar](https://github.com/basho/rebar), just add *reducks* 
to `deps` in `rebar.config`.

    {deps, [
        {reducks, ".*",  
            {git, "git://github.com/akaspin/reducks.git", "master"} }
    ]}.
    
*reducks* uses [erldis](https://github.com/cstar/erldis) to work with redis. 
*reducks*

## Basic usage

Basic usage is very simple. *reducks* working with hashsets. Feed into it the 
*key* and *function* that creates data. 

    {ok, Client} = erldis:connect(),
    Make = fun()->
            % long operation
            Data = [{<<"field">>, <<"value">>}, 
                    {<<"other">>, <<"value">>}],
            io:format("> Long operation done~n").
            {ok, Data}.
        end,
    {ok, Data} = reducks:snap(Client, <<"somekey">>, {Make}),
    {ok, Data1} = reducks:snap(Client, <<"somekey">>, {Make}),
    erldis:quit(Client),
    io:format("> One ~p~n", [Data]).
    io:format("> Two ~p~n", [Data1]).
    ...
    > Long operation done
    > One [{<<"field">>, <<"value">>}, {<<"other">>, <<"value">>}]
    > Two [{<<"field">>, <<"value">>}, {<<"other">>, <<"value">>}]
    
As we see, `Make` fun was called only once. `reducks:snap/3` will never 
replace an existing key until it is removed or expired (via `erldis:del` or 
`expire`). `reducks:snap` is synchronous operation.

`Make` is `fun` to be performed only if there is no data in the cache with 
the correct key. The result of this function must following.
    
    {ok, [{<<"fieldname">>, <<fieldvalue>>} | ... ]} % As in erldis `hmset`
                
All tuples with binary field names will be stored in cache. The order of the 
tuples in the list is not important.

## TTL

You can set expiration to hashset by adding tuple `{ttl, <expiration time>}`. 
Expiration time is `integer()` value in seconds (redis convention). 

    {ok, [ ... | {ttl, 60} ]} % One minute expiration

## Timeouts

By default *reducks* "locks" key for five minutes. It's not a problem because 
all pending processes subscribes for messages from the "worker" process that 
handles data (via [Redis PubSub](http://redis.io/commands#pubsub)). Once 
"worker" worker puts the data into the cache, all pending processes 
immediately get data too. 

You can change default timeout by adding value in tuple after `Make` fun:

    {ok, Data} = reducks:snap(Client, <<"key">>, {Make, 120000})
    
**Be careful!** If you set too small timeout, it can cause a strange behavior. 
At least in this case the function can be called twice. Avoid small timeouts. 
As I wrote earlier, large timeouts is not a problem.

## Audit

I often use [ETag](http://en.wikipedia.org/wiki/HTTP_ETag). With *reducks* you 
can audit field in hashset with `snap/4`:

    {ok, Data} = reducks:snap(Client, <<"key">>, 
            {<<"field">>, <<"expectedvalue">>}, {Make}),
    {ok, equal} = reducks:snap(Client, <<"key">>, 
            {<<"field">>, <<"expectedvalue">>}, {Make}),
            
If field in hashset coincides with expected value, `snap/4` returns 
`{ok, equal}` instead `{ok, Data}`. In all other cases, `snap/4` will behave 
like `snap/3`
 
This operation may be much less resource intensive than `snap/3`. Because 
[Redis HGET](http://redis.io/commands/hget) has complexity O(1) instead O(N) 
with [HGETAL](http://redis.io/commands/hgetall).

## Tags

You can tag keys with `reducks:snap`. Batteries included. Just add tuple 
`{tags, [binary()]}` to result of make fun.

    {ok, [... | {tags, [<<"tag-one">>, <<"tag-two">>]}]} % Two tags
    
You can then delete all the keys marked with these tags by `reducks:purge/2`

    reducks:purge(Client, [<<"tag-one">>, <<"tag-two">>])




 
