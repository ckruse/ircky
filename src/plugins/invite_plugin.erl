-module(plugins.invite_plugin).
-author("Christian Kruse <cjk@wwwtech.de>").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(irckybot_api).

% responds to INVITEs by joining the channel

init(_) -> {ok, []}.

handle_event(Msg, State) ->
    case Msg of
        {in, _ParserState, [_, _, <<"INVITE">>, _Nick, <<"#",Channel/binary>>]} ->
            irckybot_api:join(<<"#",Channel/binary>>);

        _ -> ok
    end,

    {ok, State}.

handle_call(_, State)    -> {ok, ok, State}.
handle_info(_, State)    -> {ok, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _State)     -> ok.

% eof
