-module(shorter).

-export([start/0, stop/0, restart/0, short/1, long/1]).

%% API methods
start() ->
	io:format("start called~n"),
	Pid = spawn(fun() -> loop(dict:new()) end),
	register(shorter_server, Pid), 
	Pid.

stop() ->
	io:format("stop called~n"),
	shorter_server ! stop,
	ok.

restart() ->
	stop(),
	start().

short(LongUrl) ->
	io:format("short for ~p called ~n", [LongUrl]),
	Unique = make_ref(),
	shorter_server ! {short, LongUrl, self(), Unique},
	receive
		{Unique, Ans} -> Ans
	end.

long(ShortUrl) ->
	io:format("long for ~p called ~n", [ShortUrl]),
	Unique = make_ref(),
	shorter_server ! {long, ShortUrl, self(), Unique},
	receive
		{Unique, Ans} -> Ans
	end.

%% Main Loop
loop(State) ->
	io:format("~p wait for messages ~n", [self()]),
	receive
		%% awaiting incoming LongUrl
		{short, LongUrl, From, Unique} -> 
			{Res, NewState} =
				case dict:is_key(LongUrl, State) of
					true ->
						{dict:fetch(LongUrl, State), State};
					false ->
						ShortUrl = "http://short.by/" ++ rand_str(7),
						{ShortUrl, dict:store(LongUrl, ShortUrl, State)}
				end,
			From ! {Unique, Res},
			loop(NewState);
		%% awaiting incoming LongUrl
		{long, ShortUrl, From, Unique} -> 
			FDict = dict:filter(fun(_Key, Value) -> Value =:= ShortUrl end, State),
			FList = dict:to_list(FDict),
			Res =
				case FList of
					[] -> "";
					[{Ans, _Value} | _Tail] -> Ans
				end, 
			From ! {Unique, Res},
			loop(State);
		%% awaiting stop-message
		stop -> 
			ok;
		%% all others messages - error
		Msg -> 
			io:format("error: unknown message ~p~n", [Msg]),
			loop(State)
	end.

%% Private methods
rand_str(Length) ->
	L = lists:seq(1, Length),
	lists:flatten([rand_char() || _Index <- L]).

rand_char() ->
	Chars = "qwertyuiopasdfghjklzxcvbnm",
	Index = random:uniform(length(Chars)),
	lists:nth(Index, Chars).