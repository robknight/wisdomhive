% Ejabberd module that runs yaws in embedded mode,
% and loads the ymnesia appmod for browsing mnesia.
-module(mod_mnesiaweb).
-author('rj@last.fm').
-include("yaws.hrl").
-behaviour(gen_mod).
-export([start/2, stop/1]).

start(_Host, Opts) ->
    Port = gen_mod:get_opt(port, Opts, 8001),
    code:add_path("/usr/lib/erlang/lib/yaws-1.80/ebin/"),
    application:set_env(yaws, embedded, true),
    application:start(yaws),
    GC = yaws_config:make_default_gconf(false,"yawstest"),
    SC = #sconf{
        port = Port,
        servername = "ejabnesia",
        listen = {0,0,0,0},
        appmods = [{"showdb", ymnesia}, {"wh", whweb}],
        docroot = "/var/www/vhosts/blog.wisdomhive.com/test"
        },
    yaws_api:setconf(GC, [[SC]]),
    ok.
 
stop(_Host) ->
    application:stop(yaws),
    ok.


