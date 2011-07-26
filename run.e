#!/usr/bin/escript
%% -*- erlang -*-
%%! -boot start_sasl -pa ebin -sname irckybot@localhost

main([]) ->
    main(["settings.cfg"]);

main([Filename]) ->
    io:setopts([{encoding, unicode}]),
    {ok, Settings} = file:consult(Filename),
    {ok, _IrckyBot} = irckybot_parser:new_link(Settings),
    irckybot_api:connect(),
    loop_forever().

loop_forever() ->
    receive _ -> ok end,
    loop_forever().

% eof
