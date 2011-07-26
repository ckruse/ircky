-define(SEND_TIMEOUT, 10000).    % 10sec
-define(RECV_TIMEOUT, 180000).   % 180sec

-define(CONNECT_TIMEOUT, 5000).    % wait for dns, but no more. 5s
-define(REGISTER_TIMEOUT, 30000).  % 30s
-define(RECONNECT_DELAY, 15000).   % fast reconnect 15s
-define(RETRY_DELAY, 15000).       % backoff reconnect 15s base,
                                   % delay of 0, 5, 20, 45, 80, 125
-define(QUITMSG,"Mitm Fahrrad auf die Autobahn").
-define(REALNAME,"Nick Nolte").
-define(VERSION,"0.1").

-record(state, {nick, server, connection, tries, timer, plugins}).

% eof
