-module(plugins.rockpaperscissors_plugin).
-author("Christian Kruse <cjk@wwwtech.de>").

-include("../irckybot.hrl").

-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(lists).
-import(random).
-import(irckybot_api).

init(_) -> {ok, []}.

handle_event(Msg, State) ->
    case Msg of
       {in, ParserState, Details} ->
            MyNick = ParserState#state.nick,
            BNick = list_to_binary(MyNick),
            Len = length(MyNick),

            case Details of
                [Nick, _, <<"PRIVMSG">>, <<"#",Channel/binary>>, Command] ->
                    case Command of
                        <<"!rock">> ->
                            irckybot_api:privmsg(<<"#",Channel/binary>>, [Nick, choose_hand(rock)]),
                            {ok, State};

                        <<BNick:Len/binary,": rock">> ->
                            irckybot_api:privmsg(<<"#",Channel/binary>>, [Nick, choose_hand(rock)]),
                            {ok, State};

                        <<"!paper">> ->
                            irckybot_api:privmsg(<<"#",Channel/binary>>, [Nick, choose_hand(paper)]),
                            {ok, State};
                        <<BNick:Len/binary,": paper">> ->
                            irckybot_api:privmsg(<<"#",Channel/binary>>, [Nick, choose_hand(paper)]),
                            {ok, State};

                        <<"!scissors">> ->
                            irckybot_api:privmsg(<<"#",Channel/binary>>, [Nick, choose_hand(scissors)]),
                            {ok, State};
                        <<BNick:Len/binary,": scissors">> ->
                            irckybot_api:privmsg(<<"#",Channel/binary>>, [Nick, choose_hand(scissors)]),
                            {ok, State};

                        <<"!spock">> ->
                            irckybot_api:privmsg(<<"#",Channel/binary>>, [Nick, choose_hand(spock)]),
                            {ok, State};
                        <<BNick:Len/binary,": spock">> ->
                            irckybot_api:privmsg(<<"#",Channel/binary>>, [Nick, choose_hand(spock)]),
                            {ok, State};

                        <<"!lizard">> ->
                            irckybot_api:privmsg(<<"#",Channel/binary>>, [Nick, choose_hand(lizard)]),
                            {ok, State};
                        <<BNick:Len/binary,": lizard">> ->
                            irckybot_api:privmsg(<<"#",Channel/binary>>, [Nick, choose_hand(lizard)]),
                            {ok, State};

                        _Other -> {ok, State}
                    end;

                _Other -> {ok, State}
            end;

        _Other -> {ok, State}
    end.

handle_call(_, State)    -> {ok, ok, State}.
handle_info(_, State)    -> {ok, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _)          -> ok.


choose_hand(PlayerHand) ->
    ComputerHand = get_computer_hand(),

    case get_result(PlayerHand, ComputerHand) of
        win  -> [": I chose ", atom_to_list(ComputerHand), ". You win!"];
        draw -> [": I chose ", atom_to_list(ComputerHand), ". It's a draw."];
        lose -> [": I chose ", atom_to_list(ComputerHand), ". I WIN!"]
    end.


get_computer_hand() ->
    lists:nth(random:uniform(5), [rock, paper, scissors, spock, lizard]).

%% Determine the result of an attack: we don't know a really better method, so simply make a table
get_result(Player1, Player2) ->
    case {Player1, Player2} of
        {rock, scissors}   -> win;
        {rock, lizard}     -> win;
        {paper, rock}      -> win;
        {paper, spock}     -> win;
        {scissors, paper}  -> win;
        {scissors, lizard} -> win;
        {spock, rock}      -> win;
        {spock, scissors}  -> win;
        {lizard, spock}    -> win;
        {lizard, paper}    -> win;

        {Same, Same}       -> draw;

        {_,_}              -> lose
    end.

% eof
