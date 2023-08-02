%%% Author      : Hans Svensson
%%% Description : Implement BIP0039
%%% Created     : 12 Jan 2022 by Hans Svensson
-module(ebip39).

-ifdef(TEST).
-export([read_wordlist/0, gen_mnemonic/2]).
-endif.

-export([generate_mnemonic/1,
         mnemonic_to_seed/2,
         validate_mnemonic/1]).

-define(WORDLIST, "wordlist_en.txt").

-spec mnemonic_to_seed(Mnemonic :: binary(),
                       PassPhrase :: iodata()) -> <<_:512>>.
mnemonic_to_seed(Mnemonic, PassPhrase) ->
  assert_mnemonic(Mnemonic),
  {ok, BinSeed} = epbkdf2:pbkdf2(sha512,
                                 Mnemonic,
                                 iolist_to_binary(["mnemonic", PassPhrase]),
                                 2048),
  BinSeed.

-spec generate_mnemonic(Size :: non_neg_integer()) -> binary().
generate_mnemonic(Size) ->
  case Size rem 32 of
    0 -> gen_mnemonic(Size);
    _ -> error({bad_mnemonic_size, Size, not_a_multiple_of_32})
  end.

-spec validate_mnemonic(Mnemonic :: binary()) -> ok | {error, Reason :: atom()}.
validate_mnemonic(Mnemonic) ->
  Table = word_table(),
  Ixs   = [ maps:get(W, Table) || W <- string:lexemes(Mnemonic, " ") ],
  Size  = length(Ixs) * 11,
  CSize = Size rem 32,
  SSize = (Size - CSize) div 8,
  <<Seed:SSize/bytes, CS:(CSize)>> = << <<Ix:11>> || Ix <- Ixs >>,
  case crypto:hash(sha256, Seed) of
    <<CS:(CSize), _/bitstring>> -> ok;
    _                           -> {error, checksum_error}
  end.

%% --- Internal functions
assert_mnemonic(Mnemonic) ->
  case validate_mnemonic(Mnemonic) of
    ok -> ok;
    {error, R} -> error({bad_mnemonic_string, R})
  end.


gen_mnemonic(Size) ->
  gen_mnemonic(Size, crypto:strong_rand_bytes(Size div 8)).

gen_mnemonic(Size, Seed) ->
  CSize = Size div 32,
  <<CS:(CSize), _/bitstring>> = crypto:hash(sha256, Seed),
  Table  = ix_table(),
  Words = [ maps:get(G, Table) || <<G:11>> <= <<Seed/bytes, CS:(CSize)>> ],
  iolist_to_binary(lists:join(<<" ">>, Words)).

word_table() ->
  Words = read_wordlist(),
  maps:from_list(lists:zip(Words, lists:seq(0, 2047))).

ix_table() ->
  Words = read_wordlist(),
  maps:from_list(lists:zip(lists:seq(0, 2047), Words)).

read_wordlist() ->
  File = filename:join(code:priv_dir(ebip39), ?WORDLIST),
  case file:read_file(File) of
    {ok, BinData} ->
      read_wordlist(BinData);
    {error, enotdir} -> %% In escript?
      try
        Escript        = escript:script_name(),
        {ok, Sections} = escript:extract(Escript, []),
        Archive        = proplists:get_value(archive, Sections),
        FileName       = filename:join([ebip39, priv, ?WORDLIST]),
        case zip:extract(Archive, [{file_list, [FileName]}, memory]) of
          {ok, [{_, BinData}]} -> read_wordlist(BinData);
          _                    -> error(wordlist_not_found)
        end
      catch _E:_R:_S ->
        error(wordlist_not_found)
      end
  end.

read_wordlist(BinData) ->
  Words = string:lexemes(BinData, "\n"),
  2048 = length(Words),
  Words.

