-module(irc_lib).
-author("Christian Kruse <cjk@wwwtech.de>").

-export([irc_parse/1]).

irc_parse(Line) when is_list(Line) ->
    irc_parse(list_to_binary(Line));

irc_parse(<<":", Line/binary>>) ->
    [Prefix | Rest] = re:split(Line, " ", [{parts,2}]),
    [Nick | User] = re:split(Prefix, "[!@]", [{parts,2}]),

    parse_command(Rest, [Nick, User]);

irc_parse(Line) ->
    parse_command(Line, [<<>>,<<>>]).


parse_command(Line, Acc) ->
    [Front | Trailing] = re:split(Line, " :", [{parts, 2}]),

    Parts = if
                length(Trailing) == 0 -> 16;
                true -> 15
            end,

    [Command | Params] = re:split(Front, " ", [{parts, Parts}]),

    {match, Acc ++ [Command] ++ Params ++ Trailing}.

% eof
