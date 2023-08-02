%% @doc
%% Vanillae RLP encoder/decoder
%%
%% Reference: https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/
%%
%% Agrees with Ethereum's Python implementation in randomized tests
-module(vrlp).

-export_type([decoded_data/0]).
-export([encode/1, decode/1]).

-type decoded_data() :: binary() | [decoded_data()].



-spec encode(Data) -> RLP
    when Data :: decoded_data(),
         RLP  :: binary().
%% @doc
%% encode some data

encode(Binary) when is_binary(Binary) ->
    encode_binary(Binary);
encode(List) when is_list(List) ->
    encode_list(List).



-spec encode_binary(Bytes) -> RLP
    when Bytes :: binary(),
         RLP   :: binary().
%% @private
%% encode a binary in rlp
%% @end

% single byte case when the byte is between 0..127
% result is the byte itself
encode_binary(<<Byte>>) when Byte =< 127 ->
    <<Byte>>;
% if the bytestring is 0..55 items long, the first byte is 128 + Length,
% the rest of the string is the string
encode_binary(Bytes) when byte_size(Bytes) =< 55 ->
    Size = byte_size(Bytes),
    <<(128 + Size), Bytes/binary>>;
% more than 55 bytes long, first byte is 183 + ByteLengthOfLength
% max byte size is 2^64 - 1
encode_binary(Bytes) when 55 < byte_size(Bytes), byte_size(Bytes) < (1 bsl 64) ->
    SizeInt       = byte_size(Bytes),
    SizeBytes     = binary:encode_unsigned(SizeInt, big),
    SizeOfSizeInt = byte_size(SizeBytes),
    %% 183 = 128 + 55
    %% SizeOfSizeInt > 0
    <<(183 + SizeOfSizeInt),
      SizeBytes/binary,
      Bytes/binary>>.



-spec encode_list(List) -> RLP
    when List :: [decoded_data()],
         RLP  :: binary().
%% @private
%% encode a list in rlp
%% @end

% first we encode the total payload of the list
% depending on how long it is, we then branch
encode_list(List) ->
    Payload      = << (encode(Item)) || Item <- List>>,
    Payload_Size = byte_size(Payload),
    if
        Payload_Size =< 55 ->
            <<(192 + Payload_Size), Payload/binary>>;
        55 < Payload_Size ->
            SizeBytes     = binary:encode_unsigned(Payload_Size, big),
            SizeOfSizeInt = byte_size(SizeBytes),
            %% 247 = 192 + 55
            %% SizeOfSizeInt > 0
            <<(247 + SizeOfSizeInt),
              SizeBytes/binary,
              Payload/binary>>
    end.



-spec decode(RLP) -> {Data, Rest}
    when RLP  :: binary(),
         Data :: decoded_data(),
         Rest :: binary().
%% @doc
%% decode an RLP-encoded string
%% @end

% if the first byte is between 0 and 127, that is the data
decode(<<Byte, Rest/binary>>) when Byte =< 127 ->
    {<<Byte>>, Rest};
% if the first byte is between 128 and 183 = 128 + 55, it is a bytestring and
% the length is Byte - 128
decode(<<Byte, Rest/binary>>) when Byte =< 183 ->
    PayloadByteLength = Byte - 128,
    %PayloadBitLength  = 8 * PayloadByteLength,
    %io:format("Byte              : ~p~n"
    %          "Rest              : ~w~n"
    %          "PayloadByteLength : ~p~n",
    %          %"PayloadBitLength  : ~p~n",
    %          [Byte, Rest, PayloadByteLength]),
    <<Payload:PayloadByteLength/binary,
      Rest2/binary>>  = Rest,
    {Payload, Rest2};
% If the first byte is between 184 = 183 + 1 and 191 = 183 + 8, it is a
% bytestring. The byte length of the byte length of bytestring is FirstByte -
% 183. Then pull out the actual data
decode(<<Byte, Rest/binary>>) when Byte =< 191 ->
    ByteLengthOfByteLength = Byte - 183,
    BitLengthOfByteLength  = 8 * ByteLengthOfByteLength,
    <<ByteLengthInt:BitLengthOfByteLength,
      Rest2/binary>> = Rest,
    <<Payload:ByteLengthInt/binary,
      Rest3/binary>> = Rest2,
    {Payload, Rest3};
% If the first byte is between 192 and 247 = 192 + 55, it is a list. The byte
% length of the list-payload is FirstByte - 192. Then the list payload, which
% needs to be decoded on its own.
decode(<<Byte, Rest/binary>>) when Byte =< 247 ->
    ByteLengthOfListPayload = Byte - 192,
    <<ListPayload:ByteLengthOfListPayload/binary,
      Rest2/binary>> = Rest,
    List = decode_list(ListPayload),
    {List, Rest2};
% If the first byte is between 248 = 247 + 1 and 255 = 247 + 8, it is a list.
% The byte length of the byte length of the list-payload is FirstByte - 247.
% Then the byte length of the list. Then the list payload, which needs to be
% decoded on its own.
decode(<<Byte, Rest/binary>>) ->
    ByteLengthOfByteLengthOfListPayload_int = Byte - 247,
    BitLengthOfByteLengthOfListPayload_int  = 8 * ByteLengthOfByteLengthOfListPayload_int,
    <<ByteLengthOfListPayload_int:BitLengthOfByteLengthOfListPayload_int,
      Rest2/binary>> = Rest,
    <<ListPayload_bytes:ByteLengthOfListPayload_int/binary,
      Rest3/binary>> = Rest2,
    List = decode_list(ListPayload_bytes),
    {List, Rest3}.

decode_list(<<>>) ->
    [];
decode_list(Bytes) ->
    {Item, Rest} = decode(Bytes),
    [Item | decode_list(Rest)].
