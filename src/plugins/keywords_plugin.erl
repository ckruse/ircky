-module(plugins.keywords_plugin).
-author("Christian Kruse <cjk@wwwtech.de>").

-include("../irckybot.hrl").

-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(io).
-import(io_lib).
-import(re).
-import(lists).
-import(irckybot_api).

init(_) ->
    Patterns = lists:map(
                 fun([Re,Answer]) ->
                         case re:compile(Re,[caseless]) of
                             {ok,Spec} -> [Spec,Answer];
                             {error,Err} ->
                                 io:format("error in regexp ~p: ~p~n",[Re,Err]),
                                 []
                         end
                 end,
                 [
                  ["erlang","Erlang! Fuck yeah!"],
                  ["php", "PHP? Boooooring!"],
                  ["ruby", "Ruby? Fancy!"]
                 ]
                ),
    {ok, lists:filter(
           fun([]) -> false;
              (_NotEmpty) -> true end,
           Patterns
          )}.

check_for_match(Msg,Patterns) ->
    Matches = lists:filter(fun([Re,_]) ->
                                   case re:run(Msg,Re) of
                                       {match, _} -> true;
                                       nomatch -> false
                                   end
                           end,Patterns),

    case Matches of
        [] -> ok;
        [[_,Val]|_] -> {match, Val}
    end.

add_pattern(PatternAnswer,State,Nick) ->
    [Pattern,Answer] = re:split(binary_to_list(PatternAnswer),"~~",[{parts,2}]),
    case re:compile(Pattern,[caseless]) of
        {ok,Spec} ->
            irckybot_api:notice(Nick, "Pattern successfully added"),
            State ++ [[Spec,Answer]];

        {error,Err} ->
            irckybot_api:notice(Nick, io_lib:format("Error in pattern ~p: ~p",[Pattern,Err])),
            State
    end.

handle_event(Msg, State) ->
    case Msg of
       {in, ParserState, Details} ->
            MyNick = ParserState#state.nick,
            BNick = list_to_binary(MyNick),
            Len = length(MyNick),

            case Details of
                [Nick, _, <<"PRIVMSG">>, _, <<"!add ",PatternAnswer/binary>>] ->
                    NState = add_pattern(PatternAnswer,State,Nick);

                [Nick, _, <<"PRIVMSG">>, _, <<BNick:Len/binary,": add ",PatternAnswer/binary>>] ->
                    NState = add_pattern(PatternAnswer,State,Nick);

                [Nick, _, <<"PRIVMSG">>, _, <<"!wipe">>] ->
                    irckybot_api:notice(Nick, "ok, wiped"),
                    {ok, NState} = ?MODULE:init([]);

                [Nick, _, <<"PRIVMSG">>, _, <<BNick:Len/binary,": wipe">>] ->
                    irckybot_api:notice(Nick, "ok, wiped"),
                    {ok, NState} = ?MODULE:init([]);

                [_, _, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<Saying/binary>>] ->
                    NState = State,
                    case check_for_match(binary_to_list(Saying),State) of
                        {match, Answer} -> irckybot_api:privmsg(<<"#",Channel/binary>>, Answer);
                        _Other -> ok
                    end;

                _Other ->
                    NState = State,
                    ok
            end;

        _Other ->
            NState = State,
            ok
    end,
    {ok, NState}.

handle_call(_, State)    -> {ok, ok, State}.
handle_info(_, State)    -> {ok, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _)          -> ok.

% eof
