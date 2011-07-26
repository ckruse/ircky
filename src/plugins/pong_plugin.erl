-module(plugins.pong_plugin).
-author("Christian Kruse <cjk@wwwtech.de>").

-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(io).
-import(irckybot_api).

init(_) ->
    {ok, []}.

handle_event(Msg, State) ->
    case Msg of
        {in, _ParserState, [<<>>,<<>>,<<"PING">>, Server]} ->
            irckybot_api:pong(Server),
            {ok, Server};

        _ ->
            {ok, State}
    end.


handle_call(_Request, State)        -> {ok, ok, State}.
handle_info(_Info, State)           -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State)            -> ok.

% eof
