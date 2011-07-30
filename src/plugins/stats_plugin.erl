-module(plugins.stats_plugin).
-author("Christian Kruse <cjk@wwwtech.de>").

-include("../irckybot.hrl").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(calendar).
-import(io_lib).
-import(lists).
-import(proplists).
-import(erlang).
-import(irckybot_api).

init(_Args) ->
    {ok, []}.

uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    {Day, {Hour, Min, Sec}} = calendar:seconds_to_daystime(UpTime div 1000),
    lists:flatten(io_lib:format("~p days, ~p hours, ~p minutes and ~p seconds", [Day,Hour,Min,Sec])).

memory() ->
    M = proplists:get_value(total, erlang:memory()),
    lists:flatten(io_lib:format("memory: ~p kb", [M / 1000])).

cputime() ->
    {CpuTime, _} = erlang:statistics(runtime),
    {Day, {Hour, Min, Sec}} = calendar:seconds_to_daystime(CpuTime div 1000),
    lists:flatten(io_lib:format("~p days, ~p hours, ~p minutes and ~p seconds", [Day,Hour,Min,Sec])).

handle_event(Msg, State) ->
    case Msg of
        {in, ParserState, Details} ->
            MyNick = ParserState#state.nick,
            BNick = list_to_binary(MyNick),
            Len = length(MyNick),

            case Details of
                [_, _, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!stats">>] ->
                    irckybot_api:privmsg(<<"#",Channel/binary>>, ["Uptime: ", uptime(), "; Memory: ", memory(), "; CPU time: ", cputime()]),
                    {ok, State};

                [_, _, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<BNick:Len/binary, ": stats">>] ->
                    irckybot_api:privmsg(<<"#",Channel/binary>>, ["Uptime: ", uptime(), "; Memory: ", memory(), "; CPU time: ", cputime()]),
                    {ok, State};

                [Sender, _, <<"PRIVMSG">>, <<BNick:Len/binary>>, <<"!stats">>] ->
                    irckybot_api:privmsg(Sender, ["Uptime: ", uptime(), "; Memory: ", memory(), "; CPU time: ", cputime()]),
                    {ok, State};

                _Other -> {ok, State}
            end;

        _Other ->
            {ok, State}
    end.


handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.

% eof
