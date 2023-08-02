%% @doc
%% Vanillae data composer/decomposer
%%
%% This is similar to serialization/deserialization, but not the same thing
%%
%% This code exists to work out concepts and code structure for Vanillae TS. It
%% may eventually become productized. Please do not use this right now.
%%
%% References:
%%
%% 1. https://github.com/aeternity/protocol/blob/master/serializations.md
%% 2. https://github.com/aeternity/protocol/blob/master/node/api/api_encoding.md
-module(vd).

-compile([export_all, nowarn_export_all]).

%%% TYPES

-type obj() :: #{type   := atom(),
                 vsn    := integer(),
                 fields := map()}.



-spec decompose(API_String) -> MaybeObject
    when API_String  :: string(),
         MaybeObject :: {ok, obj()}
                      | {error, Reason :: term()}.
%% @doc
%% Decompose API-encoded data

decompose("tx_" ++ Base64) -> decompose_tx_b64(Base64);
decompose(X)               -> {error, {nyi, {decompose, X}}}.




%% decode the base64 and check the hash thing
decompose_tx_b64(B64_str) ->
    B64_Bytes    = list_to_binary(B64_str),
    %% This has the double sha at the end
    Stupid_Bytes = base64:decode(B64_Bytes),
    Stupid_Size  = byte_size(Stupid_Bytes),
    %% pull apart data
    <<RLP_encoded_data : (Stupid_Size - 4) /binary,
      Check            : 4                 /binary>> = Stupid_Bytes,
    ActualDoubleSha = shasha(RLP_encoded_data),
    case Check =:= ActualDoubleSha of
        false ->
            {error, {checksum_mismatch, Check, ActualDoubleSha}};
        true ->
            decode_and_dispatch(RLP_encoded_data)
    end.



%% Double sha
shasha(Bytes) ->
    <<Result:4/binary, _/binary>> = crypto:hash(sha256, crypto:hash(sha256, Bytes)),
    Result.



%% decode rlp data
decode_and_dispatch(RLP_encoded_bytes) ->
    {DecodedData, Remainder} = vrlp:decode(RLP_encoded_bytes),
    case Remainder of
        <<>> -> decom_dispatch(DecodedData);
        _    -> {error, trailing_data}
    end.



%% at this point we have the rlp data, and based on the first field, we are
%% going to decompose the data
decom_dispatch([Tag_Bytes, Vsn_Bytes | Fields]) ->
    Tag = binary:decode_unsigned(Tag_Bytes),
    Vsn = binary:decode_unsigned(Vsn_Bytes),
    dd2(Tag, Vsn, Fields);
decom_dispatch(X) ->
    {error, {invalid_data, X}}.



%% See: https://github.com/aeternity/protocol/blob/master/serializations.md#table-of-object-tags
dd2(_Account        = 10, Vsn = 1, Fields) -> maybe(fun decompose_fields_account1/1      , 'Account'       , Vsn, Fields);
dd2(_Account        = 10, Vsn = 2, Fields) -> maybe(fun decompose_fields_account2/1      , 'Account'       , Vsn, Fields);
dd2(_SignedTx       = 11, Vsn = 1, Fields) -> maybe(fun decompose_fields_signedtx/1      , 'SignedTx'      , Vsn, Fields);
dd2(_SpendTx        = 12, Vsn = 1, Fields) -> maybe(fun decompose_fields_spendtx/1       , 'SpendTx'       , Vsn, Fields);
dd2(_ContractCallTx = 43, Vsn = 1, Fields) -> maybe(fun decompose_fields_contractcalltx/1, 'ContractCallTx', Vsn, Fields);
dd2(Tag                 , Vsn    , Fields) -> {error, {nyi, {hd2, Tag, Vsn, Fields}}}.



maybe(MaybeDecompose, Type, Vsn, Fields) ->
    case MaybeDecompose(Fields) of
        {ok, DecFields} ->
            {ok, #{type   => Type,
                   vsn    => Vsn,
                   fields => DecFields}};
        Error ->
            Error
    end.



% 10 = account, version 1
% https://github.com/aeternity/protocol/blob/master/serializations.md#accounts-version-1-basic-accounts
decompose_fields_account1([NonceBytes, BalanceBytes]) ->
    {ok, #{nonce   => binary:decode_unsigned(NonceBytes),
           balance => binary:decode_unsigned(BalanceBytes)}};
decompose_fields_account1(BadFields) ->
    {error, {invalid_account_v1_fields, BadFields}}.



% 10 = account, version 2
% https://github.com/aeternity/protocol/blob/master/serializations.md#accounts-version-2-generalized-accounts-from-fortuna-release
decompose_fields_account2([Flags,         %% :: int()
                           Nonce,         %% :: int()
                           Balance,       %% :: int()
                           GaContract,    %% :: id()
                           GaAuthFun]) -> %% :: binary()
    {ok, #{flags       => bdu(Flags),
           nonce       => bdu(Nonce),
           balance     => bdu(Balance),
           ga_contract => encode_id(GaContract),
           ga_auth_fun => GaAuthFun}};
decompose_fields_account2(BadFields) ->
    {error, {invalid_account_v2_fields, BadFields}}.



%% 11 = signedtx
decompose_fields_signedtx([Signatures, Transaction]) ->
    Sigs  = lists:map(fun encode_sg/1, Signatures),
    TxStr = encode_tx(Transaction),
    {ok, #{signatures  => Sigs,
           transaction => TxStr}};
decompose_fields_signedtx(Fields) ->
    {error, {invalid_signedtx_fields, Fields}}.



%% 12 = spendtx
%% See: https://github.com/aeternity/protocol/blob/master/serializations.md#spend-transaction
decompose_fields_spendtx([SenderBytes,
                          RecipBytes,
                          AmountBytes,
                          FeeBytes,
                          TTLBytes,
                          NonceBytes,
                          Payload]) ->
    % TODO: drop-through to make sure id humanization works
    SenderStr = encode_id(SenderBytes),
    RecipStr  = encode_id(RecipBytes),
    Amount    = binary:decode_unsigned(AmountBytes),
    Fee       = binary:decode_unsigned(FeeBytes),
    TTL       = binary:decode_unsigned(TTLBytes),
    Nonce     = binary:decode_unsigned(NonceBytes),
    {ok, #{sender    => SenderStr,
           recipient => RecipStr,
           amount    => Amount,
           fee       => Fee,
           ttl       => TTL,
           nonce     => Nonce,
           payload   => Payload}};
decompose_fields_spendtx(Fields) ->
    {error, {invalid_spendtx_fields, Fields}}.



%% 43 = contract call tx
%% See: https://github.com/aeternity/protocol/blob/master/serializations.md#contract-call-transaction
decompose_fields_contractcalltx([Caller,       % :: id()
                                 Nonce,        % :: int()
                                 Contract,     % :: id()
                                 AbiVersion,   % :: int()
                                 Fee,          % :: int()
                                 Ttl,          % :: int()
                                 Amount,       % :: int()
                                 Gas,          % :: int()
                                 GasPrice,     % :: int()
                                 CallData]) -> % :: binary()
    {ok, #{caller      => encode_id(Caller),
           nonce       => binary:decode_unsigned(Nonce),
           contract    => encode_id(Contract),
           abi_version => binary:decode_unsigned(AbiVersion),
           fee         => binary:decode_unsigned(Fee),
           ttl         => binary:decode_unsigned(Ttl),
           amount      => binary:decode_unsigned(Amount),
           gas         => binary:decode_unsigned(Gas),
           gas_price   => binary:decode_unsigned(GasPrice),
           call_data   => CallData}};
decompose_fields_contractcalltx(X) ->
    {error, {invalid_contractcalltx_fields, X}}.



%% general byte array
encode_ba(Bytes) ->
    "ba_" ++ sha64enc(Bytes).



%% contract byte array (see: https://github.com/aeternity/protocol/blob/master/node/api/api_encoding.md)
encode_cb(Bytes) ->
    "cb_" ++ sha64enc(Bytes).



%% See: https://github.com/aeternity/protocol/blob/master/serializations.md#the-id-type
%% https://github.com/aeternity/protocol/blob/master/node/api/api_encoding.md

encode_id(<<1, IdBytes:32/binary>>) -> "ak_" ++ sha58enc(IdBytes); %% ak_ account
encode_id(<<2, IdBytes:32/binary>>) -> "nm_" ++ sha58enc(IdBytes); %% nm_ name
encode_id(<<3, IdBytes:32/binary>>) -> "nm_" ++ sha58enc(IdBytes); %% cm_ commitment
encode_id(<<4, IdBytes:32/binary>>) -> "ok_" ++ sha58enc(IdBytes); %% ok_ oracle
encode_id(<<5, IdBytes:32/binary>>) -> "ct_" ++ sha58enc(IdBytes); %% ct_ contract
encode_id(<<6, IdBytes:32/binary>>) -> "ch_" ++ sha58enc(IdBytes). %% ch_ channel



encode_sg(Sig) ->
    "sg_" ++ sha58enc(Sig).



encode_tx(TxData) ->
    "tx_" ++ sha64enc(TxData).



sha58enc(Bytes) ->
    Check = shasha(Bytes),
    vb58:enc(<<Bytes/binary, Check/binary>>).



sha64enc(Bytes) ->
    Check = shasha(Bytes),
    binary_to_list(base64:encode(<<Bytes/binary, Check/binary>>)).



%% tired of typing this
bdu(X) ->
    binary:decode_unsigned(X).
