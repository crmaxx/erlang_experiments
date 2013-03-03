-module(shorter_otp).

-behaviour(gen_server).

-export([start/0, stop/0, restart/0, short/1, long/1]).

%% API methods
start() ->
	ok.

stop() ->
	ok.

restart() ->
	stop(),
	start().

short(LongUrl) ->
	ok.

long(ShortUrl) ->
	ok.

%% Main Loop


%% Private methods
rand_str(Length) ->
	L = lists:seq(1, Length),
	lists:flatten([rand_char() || _Index <- L]).

rand_char() ->
	Chars = "qwertyuiopasdfghjklzxcvbnm",
	Index = random:uniform(length(Chars)),
	lists:nth(Index, Chars).