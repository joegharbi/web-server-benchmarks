-module(websocket).
-export([out/1]).

out(A) ->
    case yaws_api:websocket_recv(A) of
        {ok, Data} ->
            % Echo back the received message
            yaws_api:websocket_send(A, Data),
            out(A);
        {error, closed} ->
            {websocket_close, normal};
        {error, Reason} ->
            {websocket_close, Reason}
    end.