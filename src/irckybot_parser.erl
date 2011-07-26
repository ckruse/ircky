-module(irckybot_parser).
-author("Christian Kruse <cjk@wwwtech.de>").

-include("irckybot.hrl").

-behaviour(gen_fsm).

%% entry points
-export([start/1, new/1, new_link/1, start_link/1]).

%% gen_fsm api
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% my states: waiting -> connecting -> registering -> ready
-export([waiting/2, connecting/2, registering/2, ready/2]).

%% shell api
new(Settings) ->
    {ok, Ref} = start(Settings),
    {ok, Ref}.

start(Settings) ->
    gen_fsm:start(?MODULE, Settings, []).

%% supervisor API
new_link(Settings) ->
    io:format("called new_link~n"),
    {ok, Ref} = start_link(Settings),
    {ok, Ref}.

start_link(Settings) ->
    io:format("called start_link~n"),
    gen_fsm:start_link(?MODULE, Settings, []).

%% gen_fsm
init(Settings) ->
    process_flag(trap_exit, true),
    register(irckybot_parser,self()),

    Nick = proplists:get_value(nickname, Settings),
    Server = proplists:get_value(server, Settings),

    {ok, Plugins} = irckybot_plugins:start_link(Settings),

    TheState = #state{nick=Nick,server=Server,tries=0,plugins=Plugins},
    {ok, waiting, TheState}.


%%
%% internal use only: makes it more readable
%%

send(Pid,Data) when is_pid(Pid) ->
    Pid ! Data;
send(_,_) ->
    ok.

send_quit(Pid) ->
    send(Pid,{send,["QUIT :", ?QUITMSG]}),
    send(Pid,quit).

send_login(Pid, Nickname) ->
    send(Pid, {send,["NICK ", Nickname]}),
    send(Pid, {send,["USER ", Nickname, " 8 * :", ?REALNAME]}).


%%%
%%% the fsm states
%%%
waiting(connect, TheState) ->
    {Host, Port} = TheState#state.server,
    Pid = irckybot_connection:start_link(self(), Host, Port),
    TheNewState = TheState#state{connection=Pid},
    io:format("connect in waiting -> connecting~n"),
    {next_state, connecting, TheNewState, ?CONNECT_TIMEOUT};

waiting({reconnect, How}, TheState) ->
    io:format("here we go: waiting({reconnect,~p})~n",[How]),
    case How of
        fast ->
            Delay = ?RECONNECT_DELAY;

        backoff ->
            Backoff = case TheState#state.tries of
                          Tries when Tries >= 5 -> 5;
                          Tries -> Tries + 1
                      end,

            Delay = Backoff * Backoff * ?RETRY_DELAY + ?RECONNECT_DELAY
    end,

    io:format("reconnect in ~p seconds~n", [Delay/1000]),
    Ref = gen_fsm:send_event_after(Delay, connect),
    TheNewState = TheState#state{tries=TheState#state.tries+1,timer=Ref},
    {next_state, waiting, TheNewState};

waiting(Ev, TheState) ->
    io:format("here we go: waiting(~p,~p)~n",[Ev,TheState]),
    {next_state, waiting, TheState}.


connecting(timeout, TheState) ->
    gen_fsm:send_event_after(0, {reconnect, fast}),
    Pid = TheState#state.connection,
    erlang:exit(Pid, kill),
    io:format("timeout in connecting -> waiting~n"),
    {next_state, waiting, TheState};

connecting(exit, TheState) ->
    gen_fsm:send_event_after(0, {reconnect, backoff}),
    io:format("connection died in connecting -> waiting~n"),
    {next_state, waiting, TheState};

connecting(success, TheState) ->
    Pid = TheState#state.connection,
    Nick = TheState#state.nick,
    send_login(Pid, Nick),
    TheNewState = TheState#state{tries=0},
    io:format("success in connecting -> registering~n"),
    {next_state, registering, TheNewState, ?REGISTER_TIMEOUT};

connecting(_Ev, TheState) ->
    {next_state, connecting, TheState, ?CONNECT_TIMEOUT}.



registering(timeout, TheState) ->
    Pid = TheState#state.connection,
    erlang:exit(Pid, kill),
    gen_fsm:send_event_after(0, {reconnect, backoff}),
    io:format("timeout: register -> waiting~n"),
    {next_state, waiting, TheState};

registering(exit, TheState) ->
    gen_fsm:send_event_after(0, {reconnect, backoff}),
    io:format("connection died: register -> waiting~n"),
    {next_state, waiting, TheState};

registering({received, Msg}, TheState) ->
    {match, IrcMessage} = irc_lib:irc_parse(Msg),
    case IrcMessage of
        [_, _, <<"001">>, _, _] ->
            %Self = irckybot_api:new(self()),
            Plugins = TheState#state.plugins,

            irckybot_plugins:notify(Plugins, online),
            irckybot_plugins:notify(Plugins, {in, TheState, IrcMessage}),

            {next_state, ready, TheState};

        _ -> {next_state, registering, TheState, ?REGISTER_TIMEOUT}
    end;

registering(_Ev, TheState) ->
    {next_state, registering, TheState, ?REGISTER_TIMEOUT}.



ready({send, Msg}, TheState) ->
    Pid = TheState#state.connection,
    Pid ! {send, Msg},
    {next_state, ready, TheState};

ready({received, Msg}, TheState) ->
    {match, IrcMessage} = irc_lib:irc_parse(Msg),
    Plugins = TheState#state.plugins,
    irckybot_plugins:notify(Plugins, {in, TheState, IrcMessage}), % notify all plugins about our message
    {next_state, ready, TheState};

ready(exit, TheState) ->
    gen_fsm:send_event_after(0, {reconnect, fast}),
    io:format("connection died: ready -> waiting~n"),
    {next_state, waiting, TheState};

ready(_Ev, TheState) ->
    {next_state, ready, TheState}.


%% death of the connection process
handle_info({'EXIT', Pid, _Reason}, StateName, #state{connection=Pid}=TheState) ->
    io:format("Pid: ~p EXITed in state: ~p~n", [Pid, StateName]),

    gen_fsm:send_event_after(0, exit),
    TheNewState = TheState#state{connection=undefined},
    {next_state, StateName, TheNewState};

handle_info(Info, StateName, TheState) ->
    %%% if StateName is connecting or registering we return a timeout
    io:format("Error: ~p ~p~n", [Info, StateName]),
    {next_state, StateName, TheState}.

handle_event(_Ev, _StateName, _TheState) ->
    {stop, "Should never happen! Please don't use gen_fsm:send_all_state_event"}.

handle_sync_event(disconnect, _From, StateName, TheState) ->
    io:format("disconnect: ~p -> waiting~n", [StateName]),
    Ref = TheState#state.timer,

    if
       is_reference(Ref) -> gen_fsm:cancel_timer(Ref);
       true -> ok
    end,

    Pid = TheState#state.connection,
    send_quit(Pid),

    TheNewState = TheState#state{tries=0,timer=undefined},
    {reply, ok, waiting, TheNewState};

%% Plugin managemenet
handle_sync_event({add_plugin, Plugin, Args}, _From, StateName, TheState) ->
    Plugins = TheState#state.plugins,
    irckybot_plugins:add_handler(Plugins, Plugin, Args),
    {reply, ok, StateName, TheState};

handle_sync_event({delete_plugin, Plugin, Args}, _From, StateName, TheState) ->
    Plugins = TheState#state.plugins,
    irckybot_plugins:delete_handler(Plugins, Plugin, Args),
    {reply, ok, StateName, TheState};

handle_sync_event(which_plugins, _From, StateName, TheState) ->
    Plugins = TheState#state.plugins,
    Reply = irckybot_plugins:which_handlers(Plugins),
    {reply, Reply, StateName, TheState};

handle_sync_event(_Ev, _From, _StateName, _TheState) ->
    {stop, "WTF?! Don't use gen_fsm:sync_send_all_state_event, fucker!"}.


code_change(_OldVsn, StateName, TheState, _Extra) ->
    Pid = TheState#state.connection,
    send(Pid,code_change),
    {ok, StateName, TheState}.

terminate(_Reason, _StateName, _TheState) -> ok.


% eof
