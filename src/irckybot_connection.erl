-module(irckybot_connection).
-author("Christian Kruse <cjk@wwwtech.de>").

-include("irckybot.hrl").

-export([start_link/3, code_change/1, connect/3]).


start_link(Parent, Host, Port)  ->
    spawn_link(?MODULE, connect, [Parent, Host, Port]).

connect(Parent, Host, Port) ->
    case gen_tcp:connect(Host, Port, [ binary, {active, true}, {packet, line}, {keepalive, true}, {send_timeout, ?SEND_TIMEOUT} ]) of
        {ok, Sock} ->
            gen_fsm:send_event(Parent, success),
            loop({Parent, Sock});
        {error, Reason} ->
            io:format("gen_tcp:connect error: ~s~n", [inet:format_error(Reason)]);
        Other ->
            io:format("gen_tcp:connect other error: ~p~n", [Other])
    end.


loop({Parent, Sock} = State) ->
    receive
        code_change ->
            ?MODULE:code_change(State);

        % data to send away on the socket
        {send, Data} ->
            io:format("send: ~p~n", [Data]),
            ok = gen_tcp:send(Sock, [Data, "\r\n"]),
            loop(State);

        % data received
        {tcp, Sock, Data} ->
            [Line|_] = re:split(Data, "\r\n"),
            io:format("received: ~p~n", [Line]),
            gen_fsm:send_event(Parent, {received, Line}),
            loop(State);

        % socket closed
        {tcp_closed, Sock} ->
            io:format("Socket ~w closed [~w]~n", [Sock, self()]);

        % socket errors
        {tcp_error, Sock, Reason} ->
            io:format("Socket ~w error: ~w [~w]~n", [Sock, Reason, self()]);

        % close socket and quit
        quit ->
            gen_tcp:close(Sock)

    after ?RECV_TIMEOUT ->
        io:format("No activity for more than ~b microseconds. Are we stuck?~n", [?RECV_TIMEOUT]),
        gen_tcp:close(Sock)
    end.

code_change(State) -> loop(State).

% eof
