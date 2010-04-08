%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the elections_2010 application.

-module(elections_2010_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for elections_2010.
start(_Type, _StartArgs) ->
    elections_2010_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for elections_2010.
stop(_State) ->
    ok.
