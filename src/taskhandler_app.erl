%%%-------------------------------------------------------------------
%% @doc taskhandler public API
%% @end
%%%-------------------------------------------------------------------

-module(taskhandler_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", task_http_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
	taskhandler_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
