% @doc Cuckoo clock with grisp led
% LED 1 is dedicated to errors and progress in standard libraries
%     off: everything started and operating normally
%     yellow: component failed
%     red: application not running
% LED 2 is dedicated to clock function:
%      blinks blue N times on full hour whenclock is N hours
% @end
-module(cuckoo).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
    grisp_led:flash(1, yellow, 300),
    grisp_led:off(2),
    case cuckoo_sup:start_link() of
        {ok, Result} ->
            grisp_led:off(1),
            {ok, Result};
        Error ->
            grisp_led:color(2, yellow),
            Error
    end.

stop(_State) ->
    grisp_led:color(1, red).
