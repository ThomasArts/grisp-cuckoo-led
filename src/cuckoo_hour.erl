-module(cuckoo_hour).

%% API
-export([start_link/0]).

start_link() ->
     {ok, spawn_link(fun() -> init() end)}.


init() ->
    timer:sleep(10000), %% sleep 10 seconds to give clock update possibility
    loop(local_hour()).

loop(HourSeen)->
    timer:sleep(1000), %% sleep 1 second
    case local_hour() rem 12 of
        HourSeen ->
            loop(HourSeen);
        0 ->
            blink(12),
            loop(0);
        Hour ->
            blink(Hour),
            loop(Hour)
    end.


%% Returns the local hour in 24 hour notation
%% return the Minutes when testing to see each minute
%% blinking of 60 modulo 12.
local_hour() ->
    Local = cuckoo_time:local_time(),
    {_, {H, _M, _S}} = calendar:system_time_to_local_time(Local, second),
    H.

blink(0) ->
    ok;
blink(N) ->
    grisp_led:color(1,blue),
    timer:sleep(500),
    grisp_led:off(1),
    timer:sleep(500),
    blink(N - 1).
