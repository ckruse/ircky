-module(plugins.nickserv_plugin).
-author("Christian Kruse <cjk@wwwtech.de>").

-include("../irckybot.hrl").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(io).
-import(irckybot_api).

init(Args) -> {ok, Args}.

handle_event(Msg, State = {Service, Password}) ->
    case Msg of
        {in, _ParserState, [_, _, <<"001">>, _Nick, _]} ->
            irckybot_api:privmsg(Service, ["identify ", Password]);
        _ ->
            ok
    end,

    {ok, State};

handle_event(_, State) ->
    {ok, State}.

handle_call(_, State)    -> {ok, ok, State}.
handle_info(_, State)    -> {ok, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _)          -> ok.

% eof
