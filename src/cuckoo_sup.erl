% @doc teleblock top level supervisor.
% @end
-module(cuckoo_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------

init([]) ->
    SupFlags =
        #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs =
        [#{id => get_time,
           start => {cuckoo_time, start_link, []},
           restart => permanent},
         #{id => cuckoo,
           start => {cuckoo_hour, start_link, []},
           restart => permanent}
        ],
    {ok, {SupFlags, ChildSpecs}}.
