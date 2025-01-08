%% Save this as `web_server.erl`
-module(web_server).
-export([start/1, stop/0, init/2, handle/2, terminate/3]).

% Start the Cowboy server
start(Port) ->
    application:start(cowboy),
    application:start(crypto),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/echo", ?MODULE, []}
        ]}
    ]),
    {ok, _Pid} = cowboy:start_clear(my_http_listener, [{port, Port}], #{env => #{dispatch => Dispatch}}),
    io:format("Web server started on port ~p~n", [Port]).

% Stop the Cowboy server
stop() ->
    cowboy:stop_listener(my_http_listener),
    io:format("Web server stopped~n").

% Init callback
init(Req, _Opts) ->
    {ok, Req, #{}}.

% Handle incoming HTTP requests
handle(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            Response = <<"Echo: ", Body/binary>>,
            {ok, Req3} = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Response, Req2),
            {ok, Req3, State};
        _ ->
            {ok, Req2} = cowboy_req:reply(405, #{<<"content-type">> => <<"text/plain">>}, <<"Method Not Allowed">>, Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.
