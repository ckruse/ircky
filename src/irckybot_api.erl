-module(irckybot_api).
-author("Christian Kruse <cjk@wwwtech.de>").

-export([tpid/0, connect/0, disconnect/0, reconnect/0, send_event/1, send_data/1, send_message/3, privmsg/2, notice/2, join/1, part/1, ping/1, pong/1, add_plugin/2, delete_plugin/2, which_plugins/0]).


tpid() ->
    whereis(irckybot_parser).

connect() ->
    gen_fsm:send_event(tpid(), connect).

disconnect() ->
    gen_fsm:sync_send_all_state_event(tpid(), disconnect).

reconnect() ->
    disconnect(),
    connect().


add_plugin(Plugin, Args) ->
    gen_fsm:sync_send_all_state_event(tpid(), {add_plugin, Plugin, Args}).

delete_plugin(Plugin, Args) ->
    gen_fsm:sync_send_all_state_event(tpid(), {delete_plugin, Plugin, Args}).

which_plugins() ->
    gen_fsm:sync_send_all_state_event(tpid(), which_plugins).


send_event(Event) ->
    gen_fsm:send_event(tpid(), Event).

send_data(Data) ->
    send_event({send, Data}).

send_message(Cmd, Destination, Msg) ->
    send_data([Cmd, " ", Destination, " :", Msg]).


privmsg(Destination, Msg) ->
    send_message("PRIVMSG", Destination, Msg).

notice(Destination, Msg) ->
    send_message("NOTICE", Destination, Msg).

join(Channel) ->
    send_data(["JOIN ", Channel]).

part(Channel) ->
    send_data(["PART ", Channel]).

ping(Server) ->
    send_data(["PING :", Server]).

pong(Server) ->
    send_data(["PONG :", Server]).

% eof
