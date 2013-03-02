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
			%% some
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
rand_string() ->
	L = list:seq(l, Length),
	list:flatten([rand_char() || _Index <- L]),

rand_char() ->
	Chars = "qwertyuiopasdfghjklzxcvbnm",
	Index = random:uniform(length(Chars)),
	list:nth(Index, Chars).