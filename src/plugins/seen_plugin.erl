-module(plugins.seen_plugin).
-author("Christian Kruse <cjk@wwwtech.de>").

-include("../irckybot.hrl").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(re).
-import(dict).
-import(lists).
-import(string).
-import(irckybot_api).

init(_) -> {ok, dict:new()}.

set_seen(To,State) ->
    Key = string:to_lower(binary_to_list(To)),

    case dict:is_key(Key, State) of
        true ->
            NState = dict:erase(Key, State),
            {ok, dict:append(Key,erlang:localtime(), NState)};

        false ->
            {ok, dict:append(Key,erlang:localtime(),State)}
    end.

get_seen(Name, State) ->
    Key = string:to_lower(binary_to_list(Name)),

    case dict:is_key(Key, State) of
        true ->
            dict:fetch(Key, State);
        false ->
            false
    end.


check_seen(Sender,Whom,State) ->
    case get_seen(Whom, State) of
        false ->
            irckybot_api:notice(Sender,["I didn't see ", Whom, " recently."]),
            {ok, State};

        Timestamp ->
            [{{Year,Month,Day},{Hour,Minute,_}}] = Timestamp,
            Msg = [
                   "Seen ",Whom," at ",
                   integer_to_list(Year),"-",integer_to_list(Month),"-",integer_to_list(Day), " ",
                   integer_to_list(Hour),":",integer_to_list(Minute)
                  ],
            irckybot_api:privmsg(Sender, Msg)
    end.

handle_event(Msg, State) ->
    case Msg of
        {in, ParserState, Details} ->
            Nick = ParserState#state.nick,
            BNick = list_to_binary(Nick),
            Len = length(Nick),

            case Details of
                [Sender, _, <<"PRIVMSG">>, _, <<"!seen ",Whom/binary>>] ->
                    check_seen(Sender,Whom,State),
                    {ok, State};
                [Sender, _, <<"PRIVMSG">>, _, <<BNick:Len/binary,": seen ", Whom/binary>>] ->
                    check_seen(Sender,Whom,State),
                    {ok, State};

                [Sender, _, <<"PRIVMSG">>, _, _] ->
                    set_seen(Sender, State);

                _Other -> {ok, State}
            end;

        _Other -> {ok, State}
    end.

handle_call(_, State)    -> {ok, ok, State}.
handle_info(_, State)    -> {ok, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _)          -> ok.

% eof
