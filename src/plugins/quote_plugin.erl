-module(plugins.quote_plugin).
-author("Christian Kruse <cjk@wwwtech.de>").

-include("../irckybot.hrl").

-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(random).
-import(file).
-import(io).
-import(lists).
-import(irckybot_api).

init(Args) ->
    random:seed(erlang:now()),
    {ok, Args}.


read_all_lines(Fd, Lines) ->
    case io:get_line(Fd, "") of
        eof  -> Lines;
        Line -> read_all_lines(Fd,Lines ++ [Line])
    end.

send_quote(File) ->
    case file:open(File, [read, read_ahead]) of
        {ok, Fd} ->
            Lines = read_all_lines(Fd,[]),
            Num = random:uniform(length(Lines)),
            {ok, lists:nth(Num,Lines)};
        _Other -> none
    end.

save_quote(File,Quote) ->
    case file:open(File, [append]) of
        {ok, Fd} ->
            io:format(Fd,"~s~n",[Quote]),
            ok;

        _Other -> ok
    end.

handle_event(Msg, State) ->
    case Msg of
       {in, ParserState, Details} ->
            MyNick = ParserState#state.nick,
            BNick = list_to_binary(MyNick),
            Len = length(MyNick),

            case Details of
                [_, _, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!quote">>] ->
                    case send_quote(State) of
                        {ok, Quote} ->
                            irckybot_api:privmsg(<<"#",Channel/binary>>, Quote);

                        none -> ok
                    end;

                [_, _, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<BNick:Len/binary,": quote">>] ->
                    case send_quote(State) of
                        {ok, Quote} ->
                            irckybot_api:privmsg(<<"#",Channel/binary>>, Quote);

                        none -> ok
                    end;

                [Nick, _, <<"PRIVMSG">>, _, <<BNick:Len/binary,": addquote ",Quote/binary>>] ->
                    save_quote(State,Quote),
                    irckybot_api:notice(Nick, ["ok, saved quote"]);

                [Nick, _, <<"PRIVMSG">>, <<BNick:Len/binary>>, <<"!addquote ",Quote/binary>>] ->
                    save_quote(State,Quote),
                    irckybot_api:notice(Nick, ["ok, saved quote"]);

                [Nick, _, <<"PRIVMSG">>, _, <<"!addquote ",Quote/binary>>] ->
                    save_quote(State,Quote),
                    irckybot_api:notice(Nick, ["ok, saved quote"]);

                _Other -> ok
            end;

        _Other -> ok
    end,
    {ok, State}.

handle_call(_, State)    -> {ok, ok, State}.
handle_info(_, State)    -> {ok, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _)          -> ok.

% eof
