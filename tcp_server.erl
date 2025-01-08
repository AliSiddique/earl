-module(tcp_server).
-export([start/1, stop/0, accept/1, handle_client/1]).

% Starts the server
start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    io:format("Server listening on port ~p~n", [Port]),
    spawn(fun() -> accept(ListenSocket) end),
    put(server_socket, ListenSocket).

% Stops the server
stop() ->
    case get(server_socket) of
        undefined ->
            io:format("Server is not running~n");
        ListenSocket ->
            gen_tcp:close(ListenSocket),
            io:format("Server stopped~n"),
            put(server_socket, undefined)
    end.

% Accepts incoming client connections
accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            io:format("Client connected~n"),
            spawn(fun() -> handle_client(ClientSocket) end),
            accept(ListenSocket);
        {error, Reason} ->
            io:format("Error accepting connection: ~p~n", [Reason])
    end.

% Handles communication with a client
handle_client(ClientSocket) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Data} ->
            io:format("Received: ~s~n", [Data]),
            gen_tcp:send(ClientSocket, <<"Echo: ", Data/binary>>),
            handle_client(ClientSocket);
        {error, closed} ->
            io:format("Client disconnected~n"),
            gen_tcp:close(ClientSocket);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            gen_tcp:close(ClientSocket)
    end.
