%%% File        : ecu_crypto.erl
%%% Author      : Hans Svensson
%%% Description :
%%% Created     : 13 Jan 2022 by Hans Svensson
-module(ecu_crypto).

-export([private_to_short/2, public_to_short/2,
         eth_sign/2, eth_recover/2, eth_verify/3, eth_msg_hash/1,
         keccak256/1]).

private_to_short(bitcoin, PrivateKey) ->
  public_to_short(bitcoin, aeu_ecdsa:private_to_public(secp256k1, PrivateKey));
private_to_short(ethereum, <<PrivateKey:256>>) ->
  public_to_short(ethereum, ecu_secp256k1:scalar_mul_base(PrivateKey)).

public_to_short(bitcoin, PubKey = <<_:33/bytes>>) ->
  crypto:hash(ripemd160, crypto:hash(sha256, PubKey));
public_to_short(bitcoin, PubKey) ->
  crypto:hash(ripemd160, crypto:hash(sha256, ecu_secp256k1:compress(PubKey)));
public_to_short(ethereum, PubKey) ->
  case PubKey of
    <<_:33/bytes>> -> public_to_short(ethereum, ecu_secp256k1:decompress(PubKey));
    <<4:8, X:256, Y:256>> -> public_to_short(ethereum, {X, Y});
    {X, Y} ->
      <<_:12/bytes, ShortPub:20/bytes>> = keccak256(<<X:256, Y:256>>),
      ShortPub
  end.

eth_sign(Msg, PrivateKey = <<_:32/bytes>>) ->
  {BaseSig, YVal} = ecu_ecdsa:sign_secp256k1(eth_msg_hash(Msg), PrivateKey),
  V = if YVal rem 2 == 0 -> 27;
         true            -> 28
      end,
  <<V:8, BaseSig/bytes>>.

eth_recover(Msg, Sig = <<_:65/bytes>>) ->
  MsgHash = eth_msg_hash(Msg),
  <<E:256>> = MsgHash,
  <<V:8, R:256, S:256>> = Sig,
  Z = E rem ecu_secp256k1:n(),
  RInv = ecu_secp256k1:s_inv(R),
  Rd = ecu_secp256k1:decompress(<<(V - 27 + 2):8, R:256>>),
  [P1, P2] =
    ecu_misc:pcomp(
      [fun() -> ecu_secp256k1:scalar_mul(ecu_secp256k1:s_mul(RInv, S), Rd) end,
       fun() -> ecu_secp256k1:scalar_mul_base(ecu_secp256k1:s_mul(RInv,Z)) end]),

  {X, Y} = ecu_secp256k1:p_add(P1, ecu_secp256k1:p_neg(P2)),
  <<_:12/bytes, RPub:20/bytes>> = keccak256(<<X:256, Y:256>>),
  RPub.

eth_verify(Msg, PublicKey, Sig) ->
  PublicKey == eth_recover(Msg, Sig).

eth_msg_hash(Msg0) ->
  Msg = ["\x19Ethereum Signed Message:\n", integer_to_list(byte_size(Msg0)), Msg0],
  keccak256(iolist_to_binary(Msg)).

keccak256(Bin) ->
  sha3:hash(256, Bin).
