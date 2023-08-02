-module(vanillae_fetcher).
-vsn("0.2.0").
-author("Craig Everett <ceverett@tsuriai.jp>").
-copyright("Craig Everett <ceverett@tsuriai.jp>").
-license("MIT").

-export([connect/4]).

-include("$zx_include/zx_logger.hrl").


connect(Node = {Host, Port}, Request, From, Timeout) ->
    Timer = erlang:send_after(Timeout, self(), timeout),
    Options = [{mode, binary}, {nodelay, true}, {active, once}],
    case gen_tcp:connect(Host, Port, Options, 3000) of
        {ok, Sock} -> do(Request, Sock, Node, From, Timer);
        Error      -> gen_server:reply(From, Error)
    end.

do(Request, Sock, Node, From, Timer) ->
    Formed = unicode:characters_to_list(form(Request, Node)),
    case gen_tcp:send(Sock, Formed) of
        ok    -> await(Sock, From, Timer);
        Error -> gen_server:reply(From, Error)
    end.

await(Sock, From, Timer) ->
    receive
        {tcp, Sock, Bin} ->
            parse(Bin, Sock, From, Timer);
        {tcp_closed, Sock} ->
            ok = erlang:cancel_timer(Timer, [{async, true}]),
            gen_server:reply(From, {error, enotconn});
        timeout ->
            gen_server:reply(From, {error, timeout})
        after 120000 ->
            gen_server:reply(From, {error, timeout})
    end.


form({get, Path}, Node) ->
    ["GET ", Path, " HTTP/1.1\r\n",
     "Host: ", host_string(Node), "\r\n",
     "User-Agent: Vanillae/0.1.0\r\n",
     "Accept: */*\r\n\r\n"];
form({post, Path, Payload}, Node) ->
    ByteSize = integer_to_list(byte_size(Payload)),
    ["POST ", Path, " HTTP/1.1\r\n",
     "Host: ", host_string(Node), "\r\n",
     "Content-Type: application/json\r\n", 
     "Content-Length: ", ByteSize, "\r\n",
     "User-Agent: Vanillae/0.1.0\r\n",
     "Accept: */*\r\n\r\n",
     Payload].


host_string({Address, Port}) when is_list(Address) ->
    PortS = integer_to_list(Port),
    [Address, ":", PortS];
host_string({Address, Port}) when is_atom(Address) ->
    AddressS = atom_to_list(Address),
    PortS = integer_to_list(Port),
    [AddressS, ":", PortS];
host_string({Address, Port}) ->
    AddressS = inet:ntoa(Address),
    PortS = integer_to_list(Port),
    [AddressS, ":", PortS].


parse(Received, Sock, From, Timer) ->
    case Received of
        <<"HTTP/1.1 200 OK\r\n", Tail/binary>> ->
            parse2(200, Tail, Sock, From, Timer);
        <<"HTTP/1.1 400 Bad Request\r\n", Tail/binary>> ->
            parse2(400, Tail, Sock, From, Timer);
        <<"HTTP/1.1 404 Not Found\r\n", Tail/binary>> ->
            parse2(404, Tail, Sock, From, Timer);
        <<"HTTP/1.1 500 Internal Server Error\r\n", Tail/binary>> ->
            parse2(500, Tail, Sock, From, Timer);
        _ ->
            ok = zx_net:disconnect(Sock),
            ok = erlang:cancel_timer(Timer, [{async, true}]),
            gen_server:reply(From, {error, {received, Received}})
    end.

parse2(Code, Received, Sock, From, Timer) ->
    case read_headers(Sock, Received) of
        {ok, Headers, Rest} -> consume(Code, Rest, Headers, Sock, From, Timer);
        Error               -> gen_server:reply(From, Error)
    end.


consume(Code, Rest, Headers, Sock, From, Timer) ->
    case maps:find(<<"content-length">>, Headers) of
        error ->
            ok = erlang:cancel_timer(Timer, [{async, true}]),
            gen_server:reply(From, {error, {headers, Headers}});
        {ok, <<"0">>} ->
            ok = erlang:cancel_timer(Timer, [{async, true}]),
            Result = case Code =:= 200 of true -> ok; false -> {error, Code} end,
            gen_server:reply(From, Result);
        {ok, Size} ->
            try
                Length = binary_to_integer(Size),
                consume2(Length, Rest, Sock, From, Timer)
            catch
                error:badarg ->
                    ok = erlang:cancel_timer(Timer, [{async, true}]),
                    gen_server:reply(From, {error, {headers, Headers}})
            end
    end.

consume2(Length, Received, Sock, From, Timer) ->
    Size = byte_size(Received),
    if
        Size == Length ->
            ok = erlang:cancel_timer(Timer, [{async, true}]),
            ok = zx_net:disconnect(Sock),
            Result = zj:decode(Received),
            gen_server:reply(From, Result);
        Size  < Length ->
            consume3(Length, Received, Sock, From, Timer);
        Size >  Length ->
            ok = erlang:cancel_timer(Timer, [{async, true}]),
            gen_server:reply(From, {error, bad_length})
    end.

consume3(Length, Received, Sock, From, Timer) ->
    ok = inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Bin} ->
            consume2(Length, <<Received/binary, Bin/binary>>, Sock, From, Timer);
        timeout ->
            gen_server:reply(From, {error, {timeout, Received}})
    end.


read_headers(Socket, <<"\r">>) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Bin} -> read_headers(Socket, <<"\r", Bin/binary>>);
        timeout            -> {error, timeout}
        after 120000       -> {error, timeout}
    end;
read_headers(_, <<"\r\n", Received/binary>>) ->
    log(info, "~p Headers died at: ~p", [?LINE, Received]),
    {error, headers};
read_headers(Socket, Received) ->
    read_hkey(Socket, Received, <<>>, #{}).

read_hkey(Socket, <<Char, Rest/binary>>, Acc, Headers)
        when $A =< Char, Char =< $Z ->
    read_hkey(Socket, Rest, <<Acc/binary, (Char + 32)>>, Headers);
read_hkey(Socket, <<Char, Rest/binary>>, Acc, Headers)
        when 32 =< Char, Char =< 57;
             59 =< Char, Char =< 126 ->
    read_hkey(Socket, Rest, <<Acc/binary, Char>>, Headers);
read_hkey(Socket, <<":", Rest/binary>>, Key, Headers) ->
    skip_hblanks(Socket, Rest, Key, Headers);
read_hkey(_, <<"\r\n", Rest/binary>>, <<>>, Headers) ->
    {ok, Headers, Rest};
read_hkey(Socket, <<>>, Acc, Headers) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Bin} -> read_hkey(Socket, Bin, Acc, Headers);
        timeout            -> {error, timeout}
        after 120000       -> {error, timeout}
    end;
read_hkey(_, Received, _, _) ->
    log(info, "~p Headers died at: ~p", [?LINE, Received]),
    {error, headers}.

skip_hblanks(Socket, <<" ", Rest/binary>>, Key, Headers) ->
    skip_hblanks(Socket, Rest, Key, Headers);
skip_hblanks(Socket, <<>>, Key, Headers) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Bin} -> skip_hblanks(Socket, Bin, Key, Headers);
        timeout            -> {error, timeout}
        after 120000       -> {error, timeout}
    end;
skip_hblanks(_, Received = <<"\r", _/binary>>, _, _) ->
    log(info, "~p Headers died at: ~p", [?LINE, Received]),
    {error, headers};
skip_hblanks(_, Received = <<"\n", _/binary>>, _, _) ->
    log(info, "~p Headers died at: ~p", [?LINE, Received]),
    {error, headers};
skip_hblanks(Socket, Rest, Key, Headers) ->
    read_hval(Socket, Rest, <<>>, Key, Headers).

read_hval(_, Received = <<"\r\n", _/binary>>, <<>>, _, _) ->
    log(info, "~p Headers died at: ~p", [?LINE, Received]),
    {error, headers};
read_hval(Socket, <<"\r\n", Rest/binary>>, Val, Key, Headers) ->
    read_hkey(Socket, Rest, <<>>, maps:put(Key, Val, Headers));
read_hval(Socket, <<Char, Rest/binary>>, Acc, Key, Headers)
        when 32 =< Char, Char =< 126 ->
    read_hval(Socket, Rest, <<Acc/binary, Char>>, Key, Headers);
read_hval(Socket, <<>>, Val, Key, Headers) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Bin} -> read_hval(Socket, Bin, Val, Key, Headers);
        timeout            -> {error, timeout}
        after 120000       -> {error, timeout}
    end;
read_hval(_, Received, _, _, _) ->
    log(info, "~p Headers died at: ~p", [?LINE, Received]),
    {error, headers}.
