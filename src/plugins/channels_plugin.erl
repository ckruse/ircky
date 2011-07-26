-module(plugins.channels_plugin).
-author("Christian Kruse <cjk@wwwtech.de>").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(io).
-import(lists).
-import(sets).
-import(irckybot_api).

init(Channels) ->
    L = lists:map(fun(X) -> list_to_binary(X) end, Channels),
    State = sets:from_list(L),
    {ok, State}.

handle_event(Msg, Channels) ->
    case Msg of
        {in, _ParserState, [_, _, <<"001">>, _Nick, _]} -> %% join the channels on connect
            lists:foreach(
              fun (Ch) -> irckybot_api:join(Ch) end,
              sets:to_list(Channels)
            ),
            {ok, Channels};

        {in, _ParserState, [_, _, <<"JOIN">>, Channel]} -> %% keep track of channels
            {ok, sets:add_element(Channel, Channels)};

        {in, _ParserState, [_, _, <<"PART">>, Channel]} -> %% keep track of channels
            {ok, sets:del_element(Channel, Channels)};

        {in, _ParserState, [_, _, <<"KICK">>, Channel|_]} -> %% keep track of channels
            {ok, sets:del_element(Channel, Channels)};

        _ -> {ok, Channels}
    end.


handle_call(_, State)    -> {ok, ok, State}.
handle_info(_, State)    -> {ok, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _State)     -> ok.

% eof
