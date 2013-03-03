-module(shorter_otp).

%% it is analog of interface
-behaviour(gen_server).

%% my public methods
-export([start/0, stop/0, restart/0, short/1, long/1]).

%% methods from gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API methods
start() ->
	gen_server:start_link({local, shurl}, shorter_otp, [], []).  

stop() ->
	gen_server:cast(shurl, stop).

restart() ->
	stop(),
	start().

short(LongUrl) ->
	gen_server:call(shurl, {short, LongUrl}).

long(ShortUrl) ->
	gen_server:call(shurl, {long, ShortUrl}).

%% gen_server API
init([]) ->
	io:format("start server~n"),
	{ok, dict:new(), dict:new()}.

handle_call({short, LongUrl}, _From, {S2L, L2S} ) ->
	{Res, NewState} =
		case dict:is_key(LongUrl, L2S) of
			true ->
				{dict:fetch(LongUrl, L2S), State};
			false ->
				ShortUrl = "http://short.by/" ++ rand_str(7),
				NewS2L = dict:store(ShortUrl, LongUrl, S2L),
				NewL2S = dict:store(LongUrl, ShortUrl, L2S),
				{ShortUrl, {NewS2L, NewL2S}}
		end,
	{reply, Res, NewState};

handle_call({long, ShortUrl}, _From, {S2L, _L2S} = State) ->
	Res = 
		case dict:is_key(ShortUrl, S2L) of
			true -> dict:fetch(ShortUrl, S2L);
			false -> ""
		end,
	{reply, Res, State};

handle_call(Msg, From, State) ->
	error_logger:error_msg("handle_call: unknow msg ~p from ~p ~n", [Msg, From]),
	{noreply, State}.

handle_cast(stop, State) ->
	io:format("normal stop~n"),
	{stop, normal, State};

handle_cast(Msg, State) ->
	error_logger:error_msg("handle_cast: unknow msg ~p ~n", [Msg]),
	{noreply, State}.

handle_info(Msg, State) ->
	error_logger:error_msg("handle_info: unknow msg ~p ~n", [Msg]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%% Private methods
rand_str(Length) ->
	L = lists:seq(1, Length),
	lists:flatten([rand_char() || _Index <- L]).

rand_char() ->
	Chars = "qwertyuiopasdfghjklzxcvbnm",
	Index = random:uniform(length(Chars)),
	lists:nth(Index, Chars).