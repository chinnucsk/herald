-module(herald_app).
-behaviour(application).
-export([start/2, stop/1, start/0]).

start() ->
    application:start(herald).

start(normal, _Args) ->
    application:start(lager),
    application:start(inets),
    APIKey = case application:get_env(herald, api_key) of
                 undefined -> 
                     lager:error("api key not found"),
                     throw(no_api_key);
                 {ok, Key} -> Key
             end,
                     
    herald_sup:start_link(APIKey).

stop(_State) ->
    init:stop(),
    ok.
