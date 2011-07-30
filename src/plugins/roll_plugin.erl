-module(plugins.roll_plugin).
-author("Christian Kruse <cjk@wwwtech.de>").

-include("../irckybot.hrl").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(random).
-import(io_lib).
-import(io).
-import(re).
-import(irckybot_api).

init(_) ->
    random:seed(erlang:now()),
    {ok, []}.

get_num(0,0) ->
    random:uniform(6);

get_num(End_S,0) ->
    End = list_to_integer(binary_to_list(End_S)),
    random:uniform(End);

get_num(Start_S,End_S) ->
    Start = list_to_integer(binary_to_list(Start_S)),
    End   = list_to_integer(binary_to_list(End_S)),
    random:uniform(abs(Start - End)) + erlang:min(Start, End).

parse_num(StartEnd) ->
    case re:run(StartEnd, "[0-9]+", [{capture, all, binary}, global]) of
        {match, [[Start_S],[End_S]|_]} ->
            get_num(Start_S,End_S);

        {match, [[End_S]]} ->
            get_num(End_S,0);

        _ ->
            get_num(0,0)
    end.


handle_event(Msg, State) ->
    case Msg of
        {in, ParserState, Lst} ->
            Nick = ParserState#state.nick,
            BNick = list_to_binary(Nick),
            Len = length(Nick),

            case Lst of
                [_, _, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!roll",StartEnd/binary>>] ->
                    Num = parse_num(StartEnd),
                    irckybot_api:privmsg(<<"#",Channel/binary>>, io_lib:format("I'm rolling a ~p", [Num]));

                [Sender, _, <<"PRIVMSG">>, <<BNick:Len/binary>>, <<"!roll",StartEnd/binary>>] ->
                    Num = parse_num(StartEnd),
                    irckybot_api:privmsg(Sender, io_lib:format("I'm rolling a ~p", [Num]));

                [_, _, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<BNick:Len/binary,": roll",StartEnd/binary>>] -> %
                    Num = parse_num(StartEnd),
                    irckybot_api:privmsg(<<"#",Channel/binary>>, io_lib:format("I'm rolling a ~p", [Num]));

                _ -> ok
            end;

        _ -> ok
    end,

    {ok, State}.

handle_call(_, State)    -> {ok, ok, State}.
handle_info(_, State)    -> {ok, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _)          -> ok.

% eof
