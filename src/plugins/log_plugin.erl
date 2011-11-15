-module(plugins.log_plugin).
-author("Christian Kruse <cjk@wwwtech.de>").

-include("../irckybot.hrl").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(calendar).
-import(io_lib).
-import(io).
-import(filelib).
-import(file).
-import(lists).
-import(proplists).
-import(erlang).
-import(re).
-import(irckybot_api).

init(Args) ->
    {ok, Args}.

irc_to_string(TStamp, Sender, Who, Target, Command, Msg) ->
    case Command of
        <<"PRIVMSG">> ->
            "[" ++ TStamp ++ "] <" ++ binary_to_list(Sender) ++ "> " ++ binary_to_list(Msg);
        <<"JOIN">> ->
            "[" ++ TStamp ++ "] -> " ++ binary_to_list(Sender) ++ " JOINED";
        <<"PART">> ->
            "[" ++ TStamp ++ "] <- " ++ binary_to_list(Sender) ++ " LEFT: " ++ binary_to_list(Msg);
        <<"QUIT">> ->
            "[" ++ TStamp ++ "] <- " ++ binary_to_list(Sender) ++ " QUIT: " ++ binary_to_list(Msg);
        <<"KICK">> ->
            "[" ++ TStamp ++ "] <- " ++ binary_to_list(Sender) ++ " KICK " ++ binary_to_list(Who) ++ ": " ++ binary_to_list(Msg);
        <<"TOPIC">> ->
            "[" ++ TStamp ++ "] ** " ++ binary_to_list(Sender) ++ " TOPIC " ++ binary_to_list(Target) ++ ": " ++ binary_to_list(Msg);
        <<"MODE">> ->
            case Msg of
                "" ->
                    "[" ++ TStamp ++ "] ** " ++ binary_to_list(Sender) ++ " MODE " ++ binary_to_list(Target) ++ ": " ++ binary_to_list(Who);
                _ ->
                    "[" ++ TStamp ++ "] ** " ++ binary_to_list(Sender) ++ " MODE " ++ binary_to_list(Who) ++ ": " ++ binary_to_list(Msg)
            end;

        _ ->
            "[" ++ TStamp ++ "] " ++ binary_to_list(Msg)
    end.

log_message(Dir, Sender, Who, Command, Target, Message) ->
    {{Year, Month, Day}, {Hours, Minutes, Seconds}} = erlang:localtime(),
    FullDir = io_lib:format("~s/~4..0B/~2..0B/~2..0B/", [Dir, Year, Month, Day]),
    TStamp = io_lib:format("~2..0B:~2..0B:~2..0B", [Hours, Minutes, Seconds]),
    filelib:ensure_dir(FullDir),
    File = FullDir ++ binary_to_list(Target) ++ ".log",

    case file:open(File, [append]) of
        {ok, Fd} ->
            io:format(Fd,"~s~n",[irc_to_string(TStamp, Sender, Who, Target, Command, Message)]);
        Other ->
            irckybot_api:privmsg(<<"#",Target/binary>>, ["Could not write to logfile ", File, ": ", Other])
    end.

check_channels(_, []) ->
    false;
check_channels(Chan, [Head | Tail]) ->
    case Chan == Head of
        true ->
            true;
        _ ->
            check_channels(Chan, Tail)
    end.

handle_event(Msg, State) ->
    case Msg of
        {in, _ParserState, Details} ->
            {Dir, Channels} = State,
            case Details of
                [Sender, _, <<"QUIT">>, Reason] ->
                    lists:map(fun(C) ->
                            log_message(Dir, Sender, <<"">>, <<"QUIT">>, list_to_binary(C), Reason)
                        end, Channels);

                [Sender, _, <<"TOPIC">>, Target, Topic] ->
                    case check_channels(binary_to_list(Target), Channels) of
                        true ->
                            log_message(Dir, Sender, <<"">>, <<"TOPIC">>, Target, Topic);
                        _ ->
                            ok
                    end;

                [Sender, _, <<"MODE">>, Target, Who, <<"">>] ->
                    case check_channels(binary_to_list(Target), Channels) of
                        true ->
                            log_message(Dir, Sender, Who, <<"MODE">>, Target, "");
                        false ->
                            ok
                    end;

                [Sender, _, <<"MODE">>, Target, Who, Reason] ->
                    case check_channels(binary_to_list(Target), Channels) of
                        true ->
                            log_message(Dir, Sender, Who, <<"MODE">>, Target, Reason);
                        false ->
                            ok
                    end;

                [Sender, _, <<"KICK">>, Target, Who, Reason] ->
                    case check_channels(binary_to_list(Target), Channels) of
                        true ->
                            log_message(Dir, Sender, Who, <<"KICK">>, Target, Reason);
                        _ ->
                            ok
                    end;

                [Sender, _, <<"JOIN">>, Target] ->
                    case check_channels(binary_to_list(Target), Channels) of
                        true ->
                            log_message(Dir, Sender, <<"">>, <<"JOIN">>, Target, <<" ">>);
                        _ ->
                            ok
                    end;

                [Sender, _, Command, Target, Message] ->
                    case check_channels(binary_to_list(Target), Channels) of
                        true ->
                            log_message(Dir, Sender, <<"">>, Command, Target, Message);
                        _ ->
                            ok
                    end;

                _ -> ok % TODO: handle everything else
            end;
        _ ->
            ok
    end,

    {ok, State}.


handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.

% eof
