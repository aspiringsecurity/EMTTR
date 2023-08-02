-module(vw).
-vsn("0.1.0").
-author("Peter Harpending <ceverett@tsuriai.jp>").
-copyright("Peter Harpending <ceverett@tsuriai.jp>").

-export([start/1]).
-compile([export_all, nowarn_export_all]).

-spec start(ArgV) -> ok
    when ArgV :: [string()].

start(ArgV) ->
    go(ArgV),
    zx:silent_stop().

%% Taking break
%%
%% WHEN back: get decompose to work

go(["help"])                   -> help();
go(["--help"])                 -> help();
go(["decompose", "ak", TxStr]) -> decompose(ak, TxStr);
go(["decompose", "tx", TxStr]) -> decompose(tx, TxStr);
go(["generate", "keypair"])    -> generate_keypair();
go(X)                          -> error({invalid_subcommand, X}).


help() ->
    io:format("you can't help people who won't help themselves~n", []).


decompose(ak, AkStr) ->
    case decompose_ak(AkStr) of
        {ok, Pubkey} -> io:format("~tw~n", [Pubkey]);
        {error, Err} -> io:format("ERROR: ~tp~n", [Err])
    end;
decompose(tx, TxStr) ->
    case vd:decompose(TxStr) of
        {ok, X}        -> io:format("~tp~n", [X]);
        {error, Error} -> io:format("ERROR: ~tp~n", [Error])
    end.


generate_keypair() ->
    #{public := PublicKey,
      secret := SecretKey} = ecu_eddsa:sign_keypair(),
    io:format("Public Key: ~w~n", [PublicKey]),
    io:format("Secret Key: ~w~n", [SecretKey]).

decompose_ak("ak_" ++ AkB58) ->
    maybe_b58_shasha_bytes(AkB58).

maybe_b58_shasha_bytes(Base58_string) ->
    With_shasha_bytes = vb58:dec(Base58_string),
    Total_size_int = byte_size(With_shasha_bytes),
    Actual_data_size_int = Total_size_int - 4,
    <<Actual_data:Actual_data_size_int/binary,
      ShaSha4:4/binary>> = With_shasha_bytes,
    case shasha4(Actual_data) =:= ShaSha4 of
        true  -> {ok, Actual_data};
        false -> {error, checksum_mismatch}
    end.

shasha4(Bytes) ->
    <<Check:4/binary, _/binary>> = crypto:hash(sha256, crypto:hash(sha256, Bytes)),
    Check.
