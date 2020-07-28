-module(cuckoo_time).

-behaviour(gen_server).

%% API
-export([start_link/0, set_local_time/0, local_time/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE,[], []).

local_time() ->
    gen_server:call(?SERVER, local_time).

set_local_time() ->
    gen_server:call(?SERVER, set_local_time).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Ideally we would like to set the grisp clock,
%% but instead we compute with the difference
%% Present Local time in seconds is grisp os:system_time(second) + clock_diff
%% where in theory clock_diff could be negative
init([]) ->
    InitState = get_worldtime(),
    {ok, TRef} =
        timer:apply_interval(60*60*1000, %% every hour
                             ?MODULE, set_local_time, []),
    {ok, InitState#{tref => TRef}}.


handle_call(local_time, _From, #{diff := Diff} = State) ->
    Local = os:system_time(second) + Diff,
    {reply, Local, State};
handle_call(set_local_time, _From, State) ->
    try Map = get_worldtime(),
         {reply, maps:get(local_time, Map), maps:merge(State, Map)}
    catch _:_ ->
            {reply, error, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{tref := TRef}) ->
    timer:cancel(TRef),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_worldtime() ->
    {ok, ClientPid} = inets:start(httpc,
				    [{profile, browser}], stand_alone),
    {ok, {{"HTTP/1.1", _, "OK"}, _, Body}} =
        httpc:request(get, {"http://worldtimeapi.org/api/ip", []}, [],
                      [{socket_opts, [{reuseaddr, true}]}],
                      ClientPid),
    Clock = os:system_time(second),
    ok = gen_server:stop(ClientPid, normal, infinity),

    Map = jsx:decode(iolist_to_binary(Body), [{labels, atom}]),
    UnixTime = maps:get(unixtime, Map, Clock),
    DST = case maps:get(dst, Map, false) of
              true -> maps:get(dst_offset, Map, 0);
              false -> 0
          end,
    Offset = maps:get(raw_offset, Map, 0),
    LocalTime = UnixTime + Offset + DST,
    #{local_time => LocalTime, diff => LocalTime - Clock}.
