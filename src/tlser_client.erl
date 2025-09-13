-module(tlser_client).

-behaviour(gen_statem).

-export([start_link/0, stop/0]).
-export([terminate/3,
         code_change/4,
         init/1,
         callback_mode/0,
         handle_event/4
        ]).

name() -> ?MODULE.

start_link() ->
    application:ensure_all_started(ssl),
    gen_statem:start_link({local, name()}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(name()).

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

init([]) ->
    io:format(user, "client > ", []),
    Opts = tlser:files() ++
           [{verify, verify_none},
            {protocol, tlser:protocol()},
            {versions, tlser:versions()},
            {ciphers, tlser:cipher_suites(client)},
            {log_level, tlser:log_level()}
           ] ++ max_fragment_length(),
    io:format(user, "client> connecting to server ~s:~p~n", [server_host(), server_port()]),
    {ok, Socket} =
        try
            ssl:connect(server_host(), server_port(), Opts, infinity)
        catch
            C:E:ST->
                error({C, E, ST})
        end,
    io:format(user, "client> connected to server~n", []),
    {ok, _State = connected, _Data = #{socket => Socket},
     [{state_timeout, 100, send}]}.

callback_mode() ->
    handle_event_function.

handle_event(state_timeout, send, _State, #{socket := Socket}) ->
    ssl:send(Socket, "ping"),
    keep_state_and_data;
handle_event(info, {ssl, Socket, Msg}, _State, #{socket := Socket}) ->
    io:format(user, "client> received message: ~ts~n", [Msg]),
    {keep_state_and_data, [{state_timeout, 5000, send}]};
handle_event(EventType, Event, _State, _Data) ->
    io:format(user, "client> ignored event: ~p: ~p~n", [EventType, Event]),
    keep_state_and_data.

server_host() ->
    case os:getenv("TLSER_SERVER_HOST") of
        false -> "localhost";
        Host -> Host
    end.

server_port() -> tlser:server_port().

max_fragment_length() ->
    case os:getenv("TLSER_MAX_FRAGMENT_LENTH") of
        false ->
            [];
        Int ->
            Max = list_to_integer(Int),
            [{max_fragment_length, Max}]
    end.
