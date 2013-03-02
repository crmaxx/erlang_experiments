-module(shorter).

-export([start/0, stop/0, short/1, long/1]).
-export([rand_char/0, rand_str/1]).

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

short(LongUrl) ->
	io:format("short for ~p called ~n", [LongUrl]),
	shorter_server ! {short, LongUrl},
	ok.

long(ShortUrl) ->
	io:format("long for ~p called ~n", [ShortUrl]),
	shorter_server ! {long, ShortUrl},
	ok.

%% Main Loop
loop(State) ->
	io:format("wait for messages ~n"),
	receive
		{short, LongUrl} -> 
			Res =
				case dict:is_key(LongUrl, State) of
					true ->
						dict:fetch(LongUrl);
					false ->
						"http://short.by/" ++ rand_str(7)
				end,
			io:format("Res ~p~n", [Res]),
			loop(State);
		{long, ShortUrl} -> 
			%% do something
			loop(State);
		stop -> 
			ok;
		Msg -> 
			io:format("error: unknown message ~p~n", [Msg]),
			loop(State)
	end.

%% Internal methods
rand_str(Length) ->
	L = lists:seq(1, Length),
	lists:flatten([rand_char() || _Index <- L]).

rand_char() ->
	Chars = "qwertyuiopasdfghjklzxcvbnm",
	Index = random:uniform(length(Chars)),
	lists:nth(Index, Chars).