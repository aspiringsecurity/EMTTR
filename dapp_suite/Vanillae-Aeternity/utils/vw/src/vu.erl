% @doc Bitcoin varuint
% https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer
-module(vu).

-compile([export_all, nowarn_export_all]).

-spec encode(Integer) -> Result
    when Integer :: integer(),
         Result  :: {ok, Encoded :: binary()}
                  | {error, Reason :: term()}.

encode(N) when N < 0 ->
    {error, {negative_N, N}};
encode(N) when N < 16#FD ->
    {ok, <<N>>};
encode(N) when N =< 16#FFFF ->
    NBytes = eu(N, 2),
    {ok, <<16#FD, NBytes/binary>>};
encode(N) when N =< 16#FFFF_FFFF ->
    NBytes = eu(N, 4),
    {ok, <<16#FE, NBytes/binary>>};
encode(N) when N < (2 bsl 64) ->
    NBytes = eu(N, 8),
    {ok, <<16#FF, NBytes/binary>>}.

% add bytes to the end as needed
eu(N, Size) ->
    Bytes = binary:encode_unsigned(N, little),
    NExtraZeros = Size - byte_size(Bytes),
    ExtraZeros = << <<0>> || _ <- lists:seq(1, NExtraZeros) >>,
    <<Bytes/binary, ExtraZeros/binary>>.



-spec decode(Encoded) -> Result
    when Encoded :: binary(),
         Result  :: {ok, Integer :: integer()}
                  | {error, Reason :: term()}.

decode(<<N>>) when N < 16#FD ->
    {ok, N};
decode(<<_, Rest/binary>>) ->
    {ok, binary:decode_unsigned(Rest, little)};
decode(_) ->
    {error, ur_mom}.


% See https://github.com/aeternity/aepp-sdk-js/blob/370f1e30064ad0239ba59931908d9aba0a2e86b6/src/utils/crypto.ts#L171-L175
salted_msg(Msg) when is_binary(Msg) ->
    P = <<"aeternity Signed Message:\n">>,
    {ok, SP}   = encode(byte_size(P)),
    {ok, SMsg} = encode(byte_size(Msg)),
    <<SP/binary,
      P/binary,
      SMsg/binary,
      Msg/binary>>.
