*reducks* is dog-pile free caching solution for structured data.

    
    application:start(redis),
    application:start(redis_ducks),
    
    Make = fun() ->
        {ok, 
            {data, [{<<"ETag">>, <<"ewkughfie">>}, 
                 {<<"Data">>, <<"Some Big Data"}>>]},
            {tags, ["one", "two"]},
            {ttl, 30}
        }
    end,
    
    {ok, [<<"ETag">>, ETag]} = redis_ducks:q("key", ["ETag"], Make),
    ...