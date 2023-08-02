%%% File        : ecu_ecdsa.erl
%%% Author      : Hans Svensson
%%% Description : ecdsa functionality
%%% Created     : 13 Jan 2022 by Hans Svensson
-module(ecu_ecdsa).

-export([sign/3, verify/4,
         sign_secp256k1/2,
         private_to_public/2]).

private_to_public(secp256k1, <<PrivateKey:256>>) ->
  ecu_secp256k1:compress(ecu_secp256k1:scalar_mul_base(PrivateKey)).

sign(secp256k1, MsgHash = <<_:32/bytes>>, PrivateKey = <<_:32/bytes>>) ->
  {Sig, _YVal} = sign_secp256k1(MsgHash, PrivateKey),
  Sig.

verify(secp256k1, MsgHash = <<_:32/bytes>>, PubKey = <<_:33/bytes>>, Sig = <<_:64/bytes>>) ->
  verify(secp256k1, MsgHash, ecu_secp256k1:decompress(PubKey), Sig);
verify(secp256k1, MsgHash = <<_:32/bytes>>, PubKey = {_, _}, Sig = <<_:64/bytes>>) ->
  <<E:256>> = MsgHash,
  <<R:256, S:256>> = Sig,
  Z = E rem ecu_secp256k1:n(),
  W = ecu_secp256k1:s_inv(S),
  [P1, P2] = ecu_misc:pcomp(
               [fun() -> ecu_secp256k1:scalar_mul_base(ecu_secp256k1:s_mul(Z, W)) end,
                fun() -> ecu_secp256k1:scalar_mul(ecu_secp256k1:s_mul(R, W), PubKey) end]),
  {X, _Y} = ecu_secp256k1:p_add(P1, P2),
  R == (X rem ecu_secp256k1:n()).

sign_secp256k1(MsgHash = <<_:32/bytes>>, PrivateKey = <<_:32/bytes>>) ->
  <<E:256>> = MsgHash,
  <<D:256>> = PrivateKey,
  Z = E rem ecu_secp256k1:n(),
  K = pick_k(secp256k1),
  {X, Y} = ecu_secp256k1:scalar_mul_base(K),
  R = X rem ecu_secp256k1:n(),
  S = ecu_secp256k1:s_mul(ecu_secp256k1:s_inv(K),
                          ecu_secp256k1:s_add(Z, ecu_secp256k1:s_mul(R, D))),
  if R == 0 orelse S == 0 ->
       sign(secp256k1, MsgHash, PrivateKey);
     true ->
       {<<R:256, S:256>>, Y}
  end.

%% --- internal functions

pick_k(secp256k1) ->
  <<K:256>> = crypto:strong_rand_bytes(32),
  case K == 0 orelse K >= ecu_secp256k1:n() of
    true  -> pick_k(secp256k1);
    false -> K
  end.
