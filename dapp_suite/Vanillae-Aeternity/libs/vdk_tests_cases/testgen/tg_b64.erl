%% base64 test generation
%-module(tg_b64).

%-compile(export_all).
%-compile(nowarn_export_all).

main([]) ->
    io:format("~ts~n", [cases(1000)]).

%% todo: write assertio function
%% assert input/output pairs

single_cases() ->
    IOPairs = [{<<>>, <<>>} | single_cases(0, [])],
    unicode:characters_to_list(
        ["[", js_pairs(IOPairs, []), "]"]
    ).


single_cases(Byte, Acc) when 0 =< Byte, Byte =< 255 ->
    ThisPair = {<<Byte>>, base64:encode(<<Byte>>)},
    NewByte  = Byte + 1,
    NewAcc   = [ThisPair | Acc],
    single_cases(NewByte, NewAcc);
single_cases(256, Acc) ->
    Acc.




cases(N) ->
    IOPairs = rand_cases(N),
    unicode:characters_to_list(
        ["[", js_pairs(IOPairs, []), "]"]
    ).


% last one
js_pairs([Pair], Acc) ->
    [Acc, js_pair(Pair)];
js_pairs([Pair | Rest], Acc) ->
    NewAcc = [Acc, js_pair(Pair), ",\n "],
    js_pairs(Rest, NewAcc).


js_pair({Base64_Decoded, Base64_Encoded}) ->
    unicode:characters_to_list(
        ["{decoded: ", encode_decoded(Base64_Decoded), ",\n"
        "  encoded: ", encode_encoded(Base64_Encoded), "}"]
    ).







rand_cases(N) ->
    rand_cases(N, []).

rand_cases(0, Acc) ->
    Acc;
rand_cases(N, Acc) when 1 =< N ->
    Input_Bytes  = rand_data(),
    Output_Bytes = base64:encode(Input_Bytes),
    NewN         = N - 1,
    NewAcc       = [{Input_Bytes, Output_Bytes} | Acc],
    rand_cases(NewN, NewAcc).



-spec rand_data() -> RandomBytes
    when RandomBytes :: binary().

rand_data() ->
    % anything between 0 and 999 bytes
    NBytes = rand:uniform(1000) - 1,
    Rand255 = fun() -> rand:uniform(256) - 1 end,
    << <<( Rand255() ):8>>
    || _ <- lists:seq(1, NBytes)
    >>.



-spec encode_decoded(Base64_Decoded) -> JS_String
    when Base64_Decoded :: binary(),
         JS_String      :: string().

encode_decoded(Bytes) ->
    unicode:characters_to_list(["new Uint8Array([",
                                encode_ns(Bytes, []),
                                "])"]).

encode_ns(<<>>, Acc) ->
    Acc;
encode_ns(<<LastByte>>, Acc) ->
    [Acc, integer_to_list(LastByte)];
encode_ns(<<Byte, Rest/binary>>, Acc) ->
    NewAcc = [Acc, integer_to_list(Byte), ", "],
    encode_ns(Rest, NewAcc).



-spec encode_encoded(Base64_Encoded) -> Js_String
    when Base64_Encoded :: binary(),
         Js_String      :: string().

encode_encoded(Base64_Encoded) ->
    unicode:characters_to_list([$", Base64_Encoded, $"]).
