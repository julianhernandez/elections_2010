%%% Copyright © 2010 Julián Hernández Gómez <julianhernandez@gmail.com>
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%% 
%%%-------------------------------------------------------------------
%%% File    : votes_collector.erl
%%% Author  : Julián Hernández Gómez <julianhernandez@gmail.com>
%%% Created : 30 Mar 2010 by Julián Hernández Gómez
%%%-------------------------------------------------------------------
-module(votes_collector).

-behaviour(gen_server).

-include_lib("twitter_client/include/twitter_client.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(COLLECT_TIMEOUT, 15*60000).

%% couchdb information
-define(COUCHDB_DB_NAME, "elections_2010").

%% candidates
-define(CANDIDATES, ["mockus", "santos", "noemi", "pardo", "vargaslleras", "petro",
		     "blanco", "araujo", "nulo"]).


-record(state, {auth}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Auth = {"mi_voto", ""},
    Args = [Auth],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Auth]) ->
    io:fwrite("init: auth -> ~p~n", [Auth]),
    {ok, #state{auth=Auth}, ?COLLECT_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, ?COLLECT_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State, ?COLLECT_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    Auth = State#state.auth,
    io:fwrite("[~p] handle_info/2: collecting replies~n", [?LINE]),
    case collect_replies(Auth) of
	[] ->
	    io:fwrite("[~p] handle_info/2: there are no new replies~n", [?LINE]),
	    ok;
	Replies ->
	    Votes = extract_votes(Replies, []),
	    io:fwrite("[~p] handle_info/2: saving votes: ~p~n", [?LINE, Votes]),
	    save_votes(Votes),
	    io:fwrite("[~p] handle_info/2: processing votes~n", [?LINE]),
	    Msg = process_votes(),
	    io:fwrite("[~p] handle_info/2: updating status: ~n~s~n", [?LINE, Msg]),
	    update_status(Auth, Msg),
	    io:fwrite("[~p] handle_info/2: notifying invalid votes~n", [?LINE]),
	    notify_invalid_votes(Auth),
	    io:fwrite("[~p] handle_info/2: done~n", [?LINE])
	end,
    {noreply, State, ?COLLECT_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
latest_reply_id() ->
    case ecouch:view_access(?COUCHDB_DB_NAME, "candidates", "latest_status_id") of
	{ok, {obj,[{"rows", [{obj, [{"key", null},
				    {"value", {obj, [{"max", Max}]}}]}]}]}} ->
	    Max;
	_ ->
	    null
    end.

collect_replies(Auth) ->
    CountArg = {"count", "200"},
    PageArg = {"page", "1"},
    SinceIdArg = case latest_reply_id() of null ->
			 {};
		     LatestReplyId ->
			 {"since_id", integer_to_list(LatestReplyId)}
		 end,
    Args = [CountArg, SinceIdArg, PageArg],
    io:fwrite("collect_replies/1: Args: ~p~n", [Args]),
    Replies = twitter_client:status_mentions(Auth, Args),
    All = collect_replies(Auth, Args, Replies, []),
    lists:reverse(All).

collect_replies(_Auth, _Args, [], Replies) ->
    io:fwrite("[~p] collect_replies/4: replies: ~p~n",
	      [?LINE, Replies]),
    Replies;
collect_replies(Auth, Args, PrevReplies, AccReplies) ->
    PrevPage = proplists:get_value("page", Args),
    Page = integer_to_list(list_to_integer(PrevPage) + 1),
    NewArgs = proplists:expand([{{"page", PrevPage}, [{"page", Page}]}], Args),
    Replies = twitter_client:status_mentions(Auth, NewArgs),
    io:fwrite("[~p] collect_replies/4: Prev: ~p~nReplies: ~p~nAcc: ~p~n",
	      [?LINE, PrevReplies, Replies, AccReplies]),
    collect_replies(Auth, Args, Replies, lists:append(PrevReplies, AccReplies)).

extract_votes([], Votes) ->
    Votes;
extract_votes([Reply|Rest], Votes) ->
    V = extract_vote(Reply),
    extract_votes(Rest, [V|Votes]).

save_votes([]) ->
    ok;
save_votes([Vote|Rest]) ->
    create_or_update_vote(Vote),
    save_votes(Rest).

process_votes() ->
    Stats = count_by_candidate(),
    format_stats(Stats).

update_status(Auth, Msg) ->
    Args = [{"status", Msg}],
    twitter_client:status_update(Auth, Args),
    ok.

notify_invalid_votes(Auth) ->
    {ok, {obj, [{"total_rows", _TotalRows}, _Offset, {"rows", Rows}]}} = 
	ecouch:view_access(?COUCHDB_DB_NAME, "candidates",
			   "voters_to_notify_of_null_vote"),
    Msg = "candidato erroneo. Valido: mockus santos noemi pardo " 
	"vargaslleras petro araujo blanco. blanco: voto en blanco",
    io:fwrite("[~p] Voters with invalid votes: ~p~n", [?LINE, Rows]),
    notify_invalid_votes(Auth, Msg, Rows).

notify_invalid_votes(_Auth, _Msg, []) ->
    ok;
notify_invalid_votes(Auth, Msg, [Row|Rest]) ->
    {obj,[{"id", _}, {"key", _},
	  {"value", {obj, Doc}}]} = Row,
    Voter = binary_to_list(proplists:get_value("_id", Doc)),
    Args = [{"user", Voter}, {"text", Msg}],
    io:fwrite("[~p] direct_new to: ~p~n", [?LINE, Args]),
    twitter_client:direct_new(Auth, Args),
    OldReplySent = {"reply_sent", false},
    ReplySent = {"reply_sent", true},
    NewDoc = proplists:expand([{OldReplySent, [ReplySent]}], Doc),
    ecouch:doc_update(?COUCHDB_DB_NAME, Voter, {obj, NewDoc}),
    notify_invalid_votes(Rest).

extract_candidate(Text) ->
    Regex = "^@mi_voto\s*:{0,1}\s*(?<candidate>[a-z]+).*",
    Options = [caseless, {capture, [candidate], list}],
    case re:run(Text, Regex, Options) of
	nomatch ->
	    null;
	{match, [Candidate]} ->
	    case string:to_lower(Candidate) of
		"mockus" -> "mockus";
		"santos" -> "santos";
		"noemi" -> "noemi";
		"pardo" -> "pardo";
		"vargaslleras" -> "vargaslleras";
		"petro" -> "petro";
		"blanco" -> "blanco";
		"araujo" -> "araujo";
		_ -> null
	    end
    end.
						 
extract_vote(Reply) ->
    {User, Text, StatusId} = {(Reply#status.user)#user.screen_name,
			      Reply#status.text,
			      Reply#status.id},
    case extract_candidate(Text) of
	null ->
	    {User, "nulo", list_to_integer(StatusId)};
	Candidate ->
	    {User, Candidate, list_to_integer(StatusId)}
    end.

create_or_update_vote(Vote) ->
    {Voter, Candidate, StatusId} = Vote,
    Doc = [{"candidate", list_to_binary(Candidate)}, {"reply_sent", false},
	   {"status_id", StatusId}],
    case ecouch:doc_get(?COUCHDB_DB_NAME, Voter) of
	{ok, {obj, [{"error", <<"not_found">>}, _]}} ->
	    io:fwrite("creating new doc: ~p~n", [Doc]),
	    ecouch:doc_create(?COUCHDB_DB_NAME, Voter, {obj, Doc});
	{ok , {obj, CurrentDoc}} ->
	    CurrentStatusId = proplists:get_value("status_id", CurrentDoc),
	    if  
		StatusId > CurrentStatusId ->
		    Rev = proplists:get_value("_rev", CurrentDoc),
		    io:fwrite("updating doc: ~p~n", [[{"_rev", Rev}|Doc]]),
		    ecouch:doc_update(?COUCHDB_DB_NAME, Voter,
				      {obj, [{"_rev", Rev}|Doc]});
		true ->
		    io:fwrite("not updating doc:  ~p < ~p ~n",
			      [StatusId, CurrentStatusId]),
		    ok
	    end
    end,
    ok.

count_by_candidate() ->
    {ok, {obj, [{"rows", Rows}]}} 
	= ecouch:view_access(?COUCHDB_DB_NAME, "candidates",
			     "count_by_candidate?group=true"),
    Votes = [{binary_to_list(C), V} || {obj, [{"key", C}, {"value", V}]} <- Rows],
    VotesPerCandidate = [{C, proplists:get_value(C, Votes, 0)} || C <- ?CANDIDATES],
    Total = lists:sum([V || {_C, V} <- VotesPerCandidate]),
    io:fwrite("Total Votes: ~p~n", [Total]),
    io:fwrite("Votes per candidate: ~p~n", [VotesPerCandidate]),
    [{total, Total}, {candidates, VotesPerCandidate}].

format_stats(Stats) ->
    Total = proplists:get_value(total, Stats),
    Candidates = proplists:get_value(candidates, Stats),
    Desc = sort_votes_desc(Candidates),
    List = lists:map(
	     fun({C, V}) -> 
		     if V =:= 0 ->
			     lists:flatten(io_lib:format("~s 0%", [C]));
			true ->
			     lists:flatten(io_lib:format("~s ~.1f%", [C, V/Total*100]))
		     end
	     end, Desc),
    CandidatesString = string:join(List, ", "),
    lists:flatten(io_lib:format("~s. Votos ~p", [CandidatesString, Total])).

sort_votes_desc(Candidates) ->
    Asc = lists:sort(fun ({_, A}, {_, B}) ->  if 
						  A =< B ->  true;
						  true -> false
					      end
		     end, Candidates),
    lists:reverse(Asc).
