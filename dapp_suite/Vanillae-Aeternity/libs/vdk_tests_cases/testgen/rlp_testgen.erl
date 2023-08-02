%-module(rlp_testgen).
%-compile(export_all).

-mode(compile).
-compile([nowarn_unused_function]).

main([]) ->
    % Decoded cases
    DecodedCases = rand_decode_datas(10_000),
    %io:format("~p~n", [DecodedCases]),
    io:format("~s~n", [format_cases_js(DecodedCases)]),
    ok.


%%%%%%%%%%%%%%%%%%%%%
%%% JS FORMATTING %%%
%%%%%%%%%%%%%%%%%%%%%

format_cases_js(Cases) ->
    format_stringlist_js(lists:map(fun format_case_js/1, Cases)).

% format a list of strings into a python list
format_stringlist_js(List) ->
    % stringlist commas are the same in python/js, so reusing python
    ["import * as rlp from './jex_include/local-vanillae-0.1.0/dist/rlp.js';\n"
     "\n"
     "type rlpcases = {decoded: rlp.decoded_data, encoded: Uint8Array};\n"
     "\n"
     "// @ts-ignore never mind; jesus christ\n"
     "export const cases : Array<rlpcases> = [\n", slcommas(List, []), "];"].

% input: decoded_data
% format a case as
% {'decoded_data': <js term for rlist>,
%  'encoded_bytes': new Uint8Array([B1, B2, B3, ...])}
format_case_js(DecodedData_rlist) ->
    % EncodedData_bytes = rlp:encode(
    % DD_js = format_data_js(DecodedData_rlist),
    EncodedData_bytes = rlp:encode(DecodedData_rlist),
    EncodedBytes_py   = format_bytes_js(EncodedData_bytes),
    DecodedData_py    = format_data_js(DecodedData_rlist),
    ["    {'decoded': ", DecodedData_py, ",\n",
     "     'encoded': ", EncodedBytes_py, "}"].


format_data_js(List) when is_list(List) ->
    format_list_js(List);
format_data_js(Bytes) when is_binary(Bytes) ->
    format_bytes_js(Bytes).


format_list_js(List) ->
    [$[, js_lcommas(List, []), $]].


% similar to commas/2 below but for a list
% cases
% - list is empty -> special case
% - exactly one element -> special case
% - two or more elements -> peel off one at a time until terminal case of exactly one
% initial input empty, so return empty
js_lcommas([], []) ->
    [];
% one item left, do not add comma
js_lcommas([Item], Acc) ->
    [Acc, format_data_js(Item)];
% two or more items left, add comma
js_lcommas([Item | Rest], Acc) ->
    js_lcommas(Rest, [Acc, format_data_js(Item), ", "]).

% format a bytestring as "bytes([Byte1, Byte2, ...])"
format_bytes_js(Bytes) ->
    % commas are the same as in python so reusing that
    Commas = commas(Bytes, []),
    ["new Uint8Array([", Commas, "])"].


%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PYTHON FORMATTING %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

format_cases_py(Cases) ->
    format_stringlist_py(lists:map(fun format_case_py/1, Cases)).

% format a list of strings into a python list
format_stringlist_py(List) ->
    ["cases = [\n", slcommas(List, []), $]].

% similar to commas/2 below but for a list of the dictstrings
% adding a comma, a newline, and a space
%
% this one does no intrnal processing
%
% cases
% - list is empty -> special case
% - exactly one element -> special case
% - two or more elements -> peel off one at a time until terminal case of exactly one
% initial input empty, so return empty
slcommas([], []) ->
    [];
% one item left, do not add comma
slcommas([Item], Acc) ->
    [Acc, Item];
% two or more items left, add comma
slcommas([Item | Rest], Acc) ->
    slcommas(Rest, [Acc, Item, ",\n"]).



% input: decoded_data
% format a case as
% {'decoded_data': <python term for rlist>,
%  'encoded_bytes': bytes([B1, B2, B3, ...])}
format_case_py(DecodedData_rlist) ->
    % EncodedData_bytes = rlp:encode(
    % DD_js = format_data_js(DecodedData_rlist),
    EncodedData_bytes = rlp:encode(DecodedData_rlist),
    EncodedBytes_py   = format_bytes_py(EncodedData_bytes),
    DecodedData_py    = format_data_py(DecodedData_rlist),
    ["    {'decoded': ", DecodedData_py, ",\n",
     "     'encoded': ", EncodedBytes_py, "}"].



format_data_py(List) when is_list(List) ->
    format_list_py(List);
format_data_py(Bytes) when is_binary(Bytes) ->
    format_bytes_py(Bytes).

format_list_py(List) ->
    [$[, lcommas(List, []), $]].

% similar to commas/2 below but for a list
% cases
% - list is empty -> special case
% - exactly one element -> special case
% - two or more elements -> peel off one at a time until terminal case of exactly one
% initial input empty, so return empty
lcommas([], []) ->
    [];
% one item left, do not add comma
lcommas([Item], Acc) ->
    [Acc, format_data_py(Item)];
% two or more items left, add comma
lcommas([Item | Rest], Acc) ->
    lcommas(Rest, [Acc, format_data_py(Item), ", "]).



% format a bytestring as "bytes([Byte1, Byte2, ...])"
format_bytes_py(Bytes) ->
    Commas = commas(Bytes, []),
    ["bytes([", Commas, "])"].

% cases:
% - bytestring is empty -> special case
% - exactly one element -> special case
% - two or more elements -> peel off one at a time until terminal case of exactly two
% empty bytestring
commas(<<>>, []) ->
    [];
% only one byte left, do not add comma
commas(<<B2>>, Acc) ->
    [Acc, integer_to_list(B2)];
% two or more bytes left: peel off one, add comma
commas(<<B, Rest/binary>>, Acc) ->
    commas(Rest, [Acc, integer_to_list(B), ", "]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% INVERSE PROPERTY TEST %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test that the inverse property is correct

test_inverse() ->
    % test stuff
    DecodedCases = rand_decode_datas(20),
    All = lists:all(% predicate
                    fun(X) -> X end,
                    % data
                    lists:map(fun(DecodedData) ->
                                  CI = check_inverse(DecodedData),
                                  ok = io:format("~p~n", [CI]),
                                  CI
                              end,
                              DecodedCases)),
    io:format("all: ~p~n", [All]),
    ok.


check_inverse(DecodedData) ->
    {DeEncodedData, <<>>} = rlp:decode(rlp:encode(DecodedData)),
    DeEncodedData =:= DecodedData.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RANDOM CASE GENERATION %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%mkcase(DecodedData) ->
%    Encoded = rlp:encode(DecodedData),
%    {Deencoded, <<>>} = rlp:decode(
%    %{decoded, DecodedData,
%    % encoded, 

% check
% - encode is correct
% - decode is the inverse of decode

% generate N-ish random decode datas
% no duplicates
rand_decode_datas(N) ->
    % hack to remove duplicate cases
    % this is good enough
    sets:to_list(sets:from_list(rand_list(N))).

% generate a list of random datas n items long
rand_list(N) ->
    [rand_data() || _ <- lists:seq(1, N)].


% generate a random bytestring or random list with 50% probability
rand_data() ->
    MkList = rand:uniform() < 0.7,
    case MkList of
        true  -> rand_list();
        false -> rand_bytes()
    end.


% generate a random list of random length between 0 and 10, inclusive
rand_list() ->
    % rand:uniform(N) is between 1 and N
    NItems = rand:uniform(3) - 1,
    rand_list(NItems).


% generate a random bytestring between 0 and 100 bytes long
rand_bytes() ->
    %works
    case rand:uniform() < 0.5 of
        % either generate between 0 and 5 items or between 5 and 100 items
        true ->
            NItems = rand:uniform(6) - 1,
            crypto:strong_rand_bytes(NItems);
        false ->
            % range between 1, 96, add 4
            NItems = (rand:uniform(96) + 4),
            crypto:strong_rand_bytes(NItems)
    end.
    % syntax error:
    % they look literally the same
    %NItems = rand:uniform(11) - 1, crypto:strong_rand:bytes(NItems).
