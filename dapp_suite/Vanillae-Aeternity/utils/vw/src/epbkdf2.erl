%%% File        : epbkdf2.erl
%%% Author      : Hans Svensson
%%% Description : Simplistic implementation of PBKDF2 on top of `crypto`
%%% Created     : 12 Jan 2022 by Hans Svensson
-module(epbkdf2).

-export([pbkdf2/4,
         pbkdf2/5,
         hmac/3]).

-spec pbkdf2(HMacHash :: crypto:hmac_hash_algorithm(),
             Pwd :: binary(),
             Salt :: binary(),
             Iterations :: non_neg_integer()) -> {ok, Key :: binary()}.
pbkdf2(HMacHash, Password, Salt, Iterations) ->
  pbkdf2(HMacHash, Password, Salt, Iterations, 64).

-spec pbkdf2(HMacHash :: crypto:hmac_hash_algorithm(),
             Pwd :: binary(),
             Salt :: binary(),
             Iterations :: non_neg_integer(),
             DerivedLength :: non_neg_integer()) -> {ok, Key :: binary()}.
pbkdf2(HMacHash, Password, Salt, Iterations, DerivedLength) ->
  Bin = pbkdf2(HMacHash, Password, Salt, Iterations, DerivedLength, 1, []),
  {ok, Bin}.

-spec hmac(HMacHash :: crypto:hmac_hash_algorithm(),
           Key :: binary(),
           Data :: binary()) -> binary().
hmac(HMacHash, Key, Data) ->
  HMAC = crypto:mac_init(hmac, HMacHash, Key),
  HMAC1 = crypto:mac_update(HMAC, Data),
  crypto:mac_final(HMAC1).

%% --- internal functions

pbkdf2(HMacHash, Password, Salt, Iterations, DerivedLength, BlockIndex, Acc) ->
  case iolist_size(Acc) > DerivedLength of
    true ->
      <<Bin:DerivedLength/binary, _/binary>> = iolist_to_binary(lists:reverse(Acc)),
      Bin;
    false ->
      Block = pbkdf2(HMacHash, Password, Salt, Iterations, BlockIndex, 1, <<>>, <<>>),
      pbkdf2(HMacHash, Password, Salt, Iterations, DerivedLength, BlockIndex + 1, [Block | Acc])
  end.

pbkdf2(_HMacHash, _Password, _Salt, Iterations, _BlockIndex, Iteration, _Prev, Acc)
    when Iteration > Iterations ->
  Acc;
pbkdf2(HMacHash, Password, Salt, Iterations, BlockIndex, 1, _Prev, _Acc) ->
  InitialBlock = hmac(HMacHash, Password, <<Salt/binary, BlockIndex:32/integer>>),
  pbkdf2(HMacHash, Password, Salt, Iterations, BlockIndex, 2, InitialBlock, InitialBlock);
pbkdf2(HMacHash, Password, Salt, Iterations, BlockIndex, Iteration, Prev, Acc) ->
  Next = hmac(HMacHash, Password, Prev),
  pbkdf2(HMacHash, Password, Salt, Iterations, BlockIndex, Iteration + 1, Next, crypto:exor(Next, Acc)).
