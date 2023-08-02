%%% File        : ecu_secp256k1.erl
%%% Author      : Hans Svensson
%%% Description : Trying to whip together a pure Erlang secp256k1
%%%               Just for usage when speed isn't crucial...
%%% Created     : 22 Dec 2021 by Hans Svensson
-module(ecu_secp256k1).

-define(P, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F).
-define(A, 16#00).
-define(B, 16#07).
-define(X, 16#79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798).
-define(Y, 16#483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8).
-define(N, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141).
-define(E, 16#7AE96A2B657C07106E64479EAC3434E99CF0497512F58995C1396C28719501EE).

-define(ADD(A, B), ((A + B) rem ?P)).
-define(MUL(A, B), ((A * B) rem ?P)).
-define(SUB(A, B), ((A - B + ?P) rem ?P)).
-define(DIV(A, B), f_div(A, B)).

-export([on_curve/1, p/0, n/0,
         scalar_mul/2, scalar_mul_base/1, p_add/2, p_neg/1,
         compress/1, decompress/1,
         f_add/2, f_mul/2, f_sub/2, f_div/2, f_inv/1,
         s_add/2, s_mul/2, s_sub/2, s_div/2, s_inv/1]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

on_curve({X, Y}) ->
  %% y^2 = x^3 + 7
  X3 = ?MUL(?MUL(X, X), X),
  Y2 = ?MUL(Y, Y),
  Y2 == ?ADD(X3, ?B).

p() -> ?P.

n() -> ?N.

scalar_mul_base(<<K:256>>) ->
  scalar_mul(K, {?X, ?Y});
scalar_mul_base(K) ->
  scalar_mul(K, {?X, ?Y}).

scalar_mul(<<K:256>>, P) ->
  scalar_mul(K, P);
scalar_mul(0, _P) ->
  {0, 0};
scalar_mul(1, P) ->
  P;
scalar_mul(K, P) ->
  case K rem 2 == 0 of
    true  -> scalar_mul(K div 2, p_add(P, P));
    false -> p_add(P, scalar_mul(K - 1, P))
  end.

compress({X, Y}) when Y rem 2 == 0 -> <<2:8, X:256>>;
compress({X, _})                   -> <<3:8, X:256>>;
compress(<<4:8, X:256, Y:256>>)    -> compress({X, Y}).

decompress(<<N:8, X:256>>) ->
  Y0 = ?B + ?MUL(X, ?MUL(X, X)),
  Y1 = pow(Y0, (?P + 1) div 4),
  case Y1 rem 2 == N rem 2 of
    true  -> {X, Y1};
    false -> {X, ?P - Y1}
  end.

p_neg({X, Y}) -> {X, ?P - Y}.

p_add(P1, {0, 0}) -> P1;
p_add({0, 0}, P2) -> P2;
p_add({X, Y1}, {X, Y2}) when Y1 /= Y2 -> {0, 0};
p_add(P = {X1, Y1}, P) ->
  M  = ?DIV(?MUL(3, ?MUL(X1, X1)), ?MUL(2, Y1)),
  X3 = ?SUB(?MUL(M, M), ?MUL(2, X1)),
  Y3 = ?SUB(?MUL(M, ?SUB(X1, X3)), Y1),
  {X3, Y3};
p_add({X1, Y1}, {X2, Y2}) ->
  M  = ?DIV(?SUB(Y2, Y1), ?SUB(X2, X1)),
  X3 = ?SUB(?MUL(M, M), ?ADD(X1, X2)),
  Y3 = ?SUB(?MUL(M, ?SUB(X1, X3)), Y1),
  {X3, Y3}.

pow(A, B) -> ecu_misc:exp_mod(A, B, ?P).

%% Arithmetics in prime field P
f_add(A, B) -> (A + B) rem ?P.
f_mul(A, B) -> (A * B) rem ?P.
f_sub(A, B) -> (A - B + ?P) rem ?P.
f_div(A, B) -> f_mul(A, f_inv(B)).

f_inv(A) ->
  pow(A, ?P - 2).

%% Arithmetics in curve group order N
s_add(A, B) -> (A + B) rem ?N.
s_mul(A, B) -> (A * B) rem ?N.
s_sub(A, B) -> (A - B + ?N) rem ?N.
s_div(A, B) -> s_mul(A, s_inv(B)).

s_inv(A) ->
  {1, S, _T} = ecu_misc:eea(A, ?N),
  (S + ?N) rem ?N.

%% curve() ->
%%   #{ p => 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F,
%%      a => 16#00, b => 16#07,
%%      x => 16#79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798,
%%      y => 16#483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8,
%%      n => 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141,
%%      e => 16#7AE96A2B657C07106E64479EAC3434E99CF0497512F58995C1396C28719501EE
%%    }.
