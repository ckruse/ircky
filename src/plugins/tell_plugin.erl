-module(plugins.tell_plugin).
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

tell(To, State) ->
    Key = string:to_lower(binary_to_list(To)),

    case dict:is_key(Key, State) of
        true ->
            Ent = dict:fetch(Key, State),
            lists:foreach(fun ({{{Year,Month,Day},{Hour,Minute,_}}, Channel, From, Message}) ->
                                  Msg = [
                                         "Message: ",
                                         integer_to_list(Year),"-",integer_to_list(Month),"-",integer_to_list(Day), " ",
                                         integer_to_list(Hour),":",integer_to_list(Minute), " ",
                                         From, " on ", Channel, ": ", Message
                                        ],

                                  irckybot_api:privmsg(To, Msg)
                          end, Ent),

            {ok, dict:erase(Key, State)};

        false ->
            {ok, State}
    end.

save(Channel, From, Message, State) ->
    Timestamp = erlang:localtime(),
    [To | Msg] = re:split(Message, "\s+", [{parts,2}]),

    irckybot_api:notice(From, ["ok, I'll  pass that to ", To, " when he/she is around."]),

    Key = string:to_lower(binary_to_list(To)),
    {ok, dict:append(Key, {Timestamp, Channel, From, Msg}, State)}.


handle_event(Msg, State) ->
    case Msg of
        {in, ParserState, Details} ->
            Nick = ParserState#state.nick,
            BNick = list_to_binary(Nick),
            Len = length(Nick),

            case Details of
                [Sender, _, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!tell ",Message/binary>>] ->
                    save(Channel, Sender, Message, State);
                [Sender, _, <<"PRIVMSG">>, <<BNick:Len/binary>>, <<"!tell ",Message/binary>>] ->
                    save(<<"Private">>, Sender, Message, State);
                [Sender, _, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<BNick:Len/binary,": tell ", Message/binary>>] ->
                    save(Channel, Sender, Message, State);

                [Sender, _, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!ask ",Message/binary>>] ->
                    save(Channel, Sender, Message, State);
                [Sender, _, <<"PRIVMSG">>, <<BNick:Len/binary>>, <<"!ask ",Message/binary>>] ->
                    save(<<"Private">>, Sender, Message, State);
                [Sender, _, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<BNick:Len/binary,": ask ", Message/binary>>] ->
                    save(Channel, Sender, Message, State);

                [Sender, _, <<"JOIN">>, <<"#",_/binary>>] ->
                    tell(Sender, State);
                [_, _, <<"NICK">>, Nick] ->
                    tell(Nick, State);
                [Sender, _, <<"PRIVMSG">>, <<"#",_/binary>>, _] ->
                    tell(Sender, State);

                _Other -> {ok, State}
            end;

        _Other -> {ok, State}
    end.

handle_call(_, State)    -> {ok, ok, State}.
handle_info(_, State)    -> {ok, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _)          -> ok.

% eof
