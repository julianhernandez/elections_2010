%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc elections_2010 startup code

-module(elections_2010).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    ensure_started(inets),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    elections_2010_sup:start_link().

%% @spec start() -> ok
%% @doc Start the elections_2010 server.
start() ->
    ensure_started(crypto),
    ensure_started(inets),
    ensure_started(ecouch),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(elections_2010).

%% @spec stop() -> ok
%% @doc Stop the elections_2010 server.
stop() ->
    Res = application:stop(elections_2010),
    application:stop(webmachine),
    application:stop(mochiweb),
    ensure_started(ecouch),
    application:stop(inets),
    application:stop(crypto),
    Res.
