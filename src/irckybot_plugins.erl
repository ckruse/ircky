-module(irckybot_plugins).
-author("Christian Kruse <cjk@wwwtech.de>").

-export([start_link/1, add_handler/3, delete_handler/3, which_handlers/1, notify/2]).

start_link(Settings) ->
    {ok, Plugins} = gen_event:start_link(),

    register(irckybot_plugins,Plugins),

    Channels = proplists:get_value(channels, Settings, []),

    add_handler(Plugins, plugins.channels_plugin, Channels),
    add_handler(Plugins, plugins.pong_plugin, []),
    add_handler(Plugins, plugins.ctcp_plugin, []),

    lists:foreach(
        fun({Plugin, Args}) ->
            add_handler(Plugins, Plugin, Args)
        end,
        proplists:get_value(plugins, Settings, [])
    ),

    {ok, Plugins}.

add_handler(GenEv, Plugin, Args)->
    case gen_event:add_handler(GenEv, Plugin, Args) of
        ok               -> ok;
        {'EXIT', Reason} -> io:format("Problem loading ~p: ~p~n", [Plugin, Reason]);
        Other            -> io:format("Loading ~p: ~p~n", [Plugin, Other])
    end.

delete_handler(GenEv, Plugin, Args)->
    case gen_event:delete_handler(GenEv, Plugin, Args) of
        ok               -> ok;
        {'EXIT', Reason} -> io:format("Problem deleting plugin ~p: ~p~n", [Plugin, Reason]);
        Other            -> io:format("Deleting ~p: ~p~n", [Plugin, Other])
    end.

notify(GenEv, Msg) ->
    gen_event:notify(GenEv, Msg).

which_handlers(GenEv) ->
    gen_event:which_handlers(GenEv).

% eof
