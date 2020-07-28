# grisp-cuckoo-led

We implement a simple application for a [GRiSP board](http://www.grisp.org). This application blinks a blue led N times for each Nth hour, like a cuckoo clock or bell tower, but without sound. GRiSP boards are programmed in the functional language [Erlang](www.erlang.org). 

## GRiSP and wall time

The GRiSP board contains a clock, but version 1 of this board does not have a battery to keep the time settings. Moreover, Erlang has no primitve to set the clock; the langauge considers it an operating system task to set the clock. It can however read the clock:

```erlang
1> os:system_time(second).
567994227
```
This is the time on the GRiSP in seconds after it has been running for a few minutes. Using a website like 
https://www.epochconverter.com/ we can see that this is 33 years ago.
```
Your time zone: Friday, 1 January 1988 01:10:27 GMT+01:00
```

## Performing periodic tasks

There are plenty of applications of a GRiSP board that would benefit from knowing the wall time. For example, if you letit take control over your primitive robot vacuum cleaner or the plant watering system in the shed. Youmay want to say, "perform this task at 10:00 each day". You don't necessarily mean ten o'clock on the second,  but somewhere close to that time.

We could write some software to set the clock via a webpage. Just implement a web server, make a nice GUI, send the actual time and store it. Storing would require to dive in C code...  not for me, thanks. Moreover, we would need a database for daylight-saving times all over the world and update that database regularly.

We could try to forget about the web interface and just set the time when we deploy the software, but without a battery and with daylight-saving time issues, that would not work very well.

We could buy some more hardware.... but that takes yet another slot.

We have the code in this repository to our rescue. It simply gets the time from http://worldtimeapi.org and remembers the difference. It gets the time once every hour, such that daylight saving time updates will make it. If the internet connection fails, it will try again an hour later. The build-in clock is used to tick the time, the difference is added in our local call to get the time:

```erlang
2> cuckoo_time:local_time().
1595927861
```
Which translates to: `Your time zone: Tuesday, 28 July 2020 11:17:41 GMT+02:00 DST`

There is some imprecision, for sure: [try it](http://worldtimeapi.org/api/ip). It is not NTP, but fair enough.
Note that we get the time based on the IP address exposed to the worltime server. If you are behind a VPN, this might be confusing. 
By querying only once an hour, we may. be a bit off around a switch to or from daylight-saving time. Whether that is an issue is very much on what you want to do with this wall time.

## The setup

We use a minimal setup to show the point. I even removed the possibility to connect a remote Erlang node to the GRiSP board. Very basic.
The kind of minimum you need is a **`rebar.config`** file that tells the rebar3 build tool for Erlang how to build the software and how to package it to put on the microSD that is inserted in the GRiSP.

### rebar.config

In the `rebar.config` file we put dependencies to other project. We need the `grisp` software as well as a web client to perform http requests to the time server. Erlang comes with a simple http client and we use that one. The time server responds with a json object like:
```json
abbreviation	"CEST"
client_ip	"X.X.X.X"
datetime	"2020-07-28T09:32:42.309700+02:00"
day_of_week	2
day_of_year	210
dst	true
dst_from	"2020-03-29T01:00:00+00:00"
dst_offset	3600
dst_until	"2020-10-25T01:00:00+00:00"
raw_offset	3600
timezone	"Europe/Amsterdam"
unixtime	1595921562
utc_datetime	"2020-07-28T07:32:42.309700+00:00"
utc_offset	"+02:00"
week_number	31
```
In order to parse that json we use the package `jsx`. 

### GRiSP config

GRiSP expects a few files to be placed in `grisp/grisp_base/files`. These files configure, for example the network interface. We want the GRiSP board to get an IPaddress from a local wifi and we hard-code the SSID and password in `wpa_supplicant.conf`. **Make sure you change [this file](grisp/grisp_base/files/wpa_supplicant.conf) with your personal settings**.

The `erl_initrc` file need to contain a name server and dns details in order to have the web client talk to a named webservice:
```erlang

{nameserver, {8,8,8,8}}.

%% Use Erlang DNS client
{edns, 0}.

% Specify lookup method
{lookup, [file, dns]}.
``` 

### application settings

In Erlang, applications have their own little [`.app` file](src/cuckoo.app.src) in which details of the application are provided. It is important that this file contains the dependencies, such that when building a release, the dependencies are also copied and started.

```erlang
{application, cuckoo, [
    {description, "A GRiSP application to blink a led N times each Nth hour"},
    {vsn, "1.0.0"},
    {registered, []},
    {mod, {cuckoo, []}},
    {applications, [
        kernel,
        stdlib,
        inets,
        jsx,
        grisp
    ]},
    {env,[]},
    {modules, []},

    {maintainers, ["Thomas Arts"]},
    {licenses, ["MIT"]},
    {links, []}
]}.
```

# The Code

Erlang applications are organized by supervision trees. There is one module [cuckoo.erl](src/cuckoo.erl) that implements the logic of starting and stopping an application. This starts the top node of the supervision tree [cuckoo_sup.erl](src/cuckoo_sup.erl). The code in those two modules is standard code. There is a bit of playing with the GRiSP leds to indicate. what went wrong when an error occurs in the startup phase, but nothing more.

The supervision tree starts and monitors two processes. The first one [cuckoo_time.erl](src/cuckoo_time.erl]) periodically grabs the time from the internet service worldtimeapi.org. This time is compared to the local grisp clock and the differnece is kept in the state of this process:
```erlang
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
```
We start a web client, send a request, record the GRiSP local time directly after receiving the response and then stop the web client.

We decode the json object that is returned by the server and get the properties `unixtime`, `dst` and in case DST is true, we get the `dst_offset`. The unix time is in seconds and points to universal time. There is a `raw_offset` that indicates how far we are from universal time and together with the DST offset we get the local time in seconds.

The second. processes started by. the supervision tree [cuckoo_hour.erl](src/cuckoo_hour.erl) checks the local clock (which is far cheaper than querying the internet) each second. We compute the hour from the local time using a standard library functions:
```erlang
local_hour() ->
    Local = cuckoo_time:local_time(),
    {_, {H, _M, _S}} = calendar:system_time_to_local_time(Local, second),
    H. 
``` 
We could equally well get the day or month back from that tuple and perform an action each day, each month or each minute.

As soon as we see an hour that we have not seen before, we blink, otherwise, we just loop to wait yet another second.
```erlang
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
```

Note that there is a subtle match for `0`, because we want to blink 12 times in case of midnight and mid-day.
