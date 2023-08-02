%%% File        : ecu_ed25519.erl
%%% Author      : Hans Svensson
%%% Description : Trying to whip together a pure Erlang ed25519
%%%               Just for usage when speed isn't crucial...
%%% Created     : 13 Jan 2022 by Hans Svensson
-module(ecu_ed25519).

-define(P, 16#7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFED).
-define(N, 16#1000000000000000000000000000000014DEF9DEA2F79CD65812631A5CF5D3ED).

-type pt_affine() :: {non_neg_integer(), non_neg_integer()}. %% {X, Y}
-type pt_hom_ext() :: {non_neg_integer(), non_neg_integer(),
                       non_neg_integer(), non_neg_integer()}. %% {X, Y, Z, T}
-type pt_compressed() :: <<_:256>>. %% Y coord + odd/even X.

-type pt() :: pt_affine() | pt_hom_ext() | pt_compressed().

%% -type fld_elem() :: 0..(?P-1).
-type scalar() :: 0..(?N-1).

-define(D, 16#52036CEE2B6FFE738CC740797779E89800700A4D4141D8AB75EB4DCA135978A3).
-define(X, 16#216936D3CD6E53FEC0A4E231FDD6DC5C692CC7609525A7B2C9562D608F25D51A).
-define(Y, 16#6666666666666666666666666666666666666666666666666666666666666658).
-define(XY, 16#67875F0FD78B766566EA4E8E64ABE37D20F09F80775152F56DDE8AB3A5B7DDA3).
-define(GA, {?X, ?Y}).
-define(GE, {?X, ?Y, 1, ?XY}).

-define(PPLUS3DIV8, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE).
-define(TWOPOWPMINUS1DIV4, 16#2B8324804FC1DF0B2B4D00993DFBD7A72F431806AD2FE478C4EE1B274A0EA0B0).

-define(ADD(A, B), ((A + B) rem ?P)).
-define(MUL(A, B), ((A * B) rem ?P)).
-define(SUB(A, B), ((A - B + ?P) rem ?P)).
-define(DIV(A, B), f_div(A, B)).

-export_type([pt/0, scalar/0]).

-export([on_curve/1, p/0, n/0, pt_eq/2,
         scalar_mul/2, scalar_mul_base/1,
         scalar_mul_noclamp/2, scalar_mul_base_noclamp/1,
         scalar_reduce/1,
         p_add/2, p_sub/2, p_neg/1, p_dbl/1,
         compress/1, decompress/1,
         f_add/2, f_mul/2, f_sub/2, f_div/2, f_inv/1,
         s_add/2, s_mul/2, s_sub/2, s_div/2, s_inv/1]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-spec pt_eq(P1 :: pt(), P2 :: pt()) -> boolean().
pt_eq({X1, Y1}, {X2, Y2}) ->
  X1 == X2 andalso Y1 == Y2;
pt_eq(C1, C2) when is_binary(C1), is_binary(C2) ->
  C1 == C2;
pt_eq({X1, Y1, Z1, _T1}, {X2, Y2, Z2, _T2}) ->
  ?SUB(?MUL(X1, Z2), ?MUL(X2, Z1)) == 0
  andalso ?SUB(?MUL(Y1, Z2), ?MUL(Y2, Z1)) == 0;
pt_eq(P1, P2) ->
  pt_eq(to_ext_hom(P1), to_ext_hom(P2)).

%% Libsodium has additional checks
%%  - weak keys
%%  - canonical representation
%%  - main subgroup...
on_curve({X, Y}) ->
  X2 = ?MUL(X, X),
  Y2 = ?MUL(Y, Y),
  LHS = ?SUB(Y2, X2),
  RHS = ?ADD(1, ?MUL(?D, ?MUL(X2, Y2))),
  0 == ?SUB(LHS, RHS);
on_curve(P) ->
  on_curve(to_affine(P)).

p() -> ?P.

n() -> ?N.

-define(TWO_POW_255_MINUS_1, 16#7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).

-spec compress(P :: pt()) -> pt_compressed().
compress(<<_:32/binary>> = P) -> P;
compress({_, _, _, _} = P)    -> compress(to_affine(P));
compress({X, Y}) ->
  V = (Y band ?TWO_POW_255_MINUS_1) bor ((X band 1) bsl 255),
  <<V:256/little>>.

-spec decompress(pt_compressed()) -> pt_hom_ext().
decompress(<<Y0:256/little>>) ->
  X0 = Y0 bsr 255,
  Y  = Y0 band ?TWO_POW_255_MINUS_1,
  X  = xrecover(Y),
  case X rem 2 == X0 of
    true  -> to_ext_hom({X, Y});
    false -> to_ext_hom({?P - X, Y})
  end.

p_neg({X, Y})       -> {?P - X, Y};
p_neg({X, Y, Z, T}) -> {?P - X, Y, Z, ?P - T};
p_neg(P)            -> p_neg(to_ext_hom(P)).

p_sub(P1, P2) -> p_add(P1, p_neg(P2)).

-spec p_add(X :: pt(), Y :: pt()) -> pt_hom_ext().
p_add({X1, Y1, Z1, T1}, {X2, Y2, Z2, T2}) ->
  A = ?MUL(?SUB(Y1, X1), ?SUB(Y2, X2)),
  B = ?MUL(?ADD(Y1, X1), ?ADD(Y2, X2)),
  C = ?MUL(?MUL(T1, T2), 2 * ?D),
  D = ?MUL(2 * Z1, Z2),
  E = ?SUB(B, A),
  F = ?SUB(D, C),
  G = ?ADD(D, C),
  H = ?ADD(A, B),
  {?MUL(E, F), ?MUL(G, H), ?MUL(F, G), ?MUL(E, H)};
p_add(P1, P2) ->
  p_add(to_ext_hom(P1), to_ext_hom(P2)).

p_dbl({X, Y, Z, _T}) ->
  A = ?MUL(X, X),
  B = ?MUL(Y, Y),
  C = ?MUL(2 * Z, Z),
  D = ?P - A,
  XY = X + Y,
  E = ?SUB(?MUL(XY, XY), ?ADD(A, B)),
  G = ?ADD(D, B),
  F = ?SUB(G, C),
  H = ?SUB(D, B),
  {?MUL(E, F), ?MUL(G, H), ?MUL(F, G), ?MUL(E, H)};
p_dbl(P) ->
  p_dbl(to_ext_hom(P)).

-spec scalar_mul_base(Scalar :: scalar() | binary()) -> pt_hom_ext().
scalar_mul_base(<<K:256/little>>) ->
  scalar_mul_(clamp(K), ?GE);
scalar_mul_base(K) when is_integer(K), K >= 0, K < ?N ->
  scalar_mul_(clamp(K), ?GE).

-spec scalar_mul(Scalar :: scalar() | binary(), Pt :: pt()) -> pt_hom_ext().
scalar_mul(<<K:256/little>>, P) ->
  scalar_mul(K, P);
scalar_mul(K, P) ->
  scalar_mul_(clamp(K), to_ext_hom(P)).

-spec scalar_mul_base_noclamp(Scalar :: scalar() | binary()) -> pt_hom_ext().
scalar_mul_base_noclamp(<<K:256/little>>) ->
  scalar_mul_(K, ?GE);
scalar_mul_base_noclamp(K) when is_integer(K), K >= 0, K < ?N ->
  scalar_mul_(K, ?GE).

-spec scalar_mul_noclamp(Scalar :: scalar() | binary(), Pt :: pt()) -> pt_hom_ext().
scalar_mul_noclamp(<<K:256/little>>, P) ->
  scalar_mul_noclamp(K, P);
scalar_mul_noclamp(K, P) ->
  scalar_mul_(K, to_ext_hom(P)).

to_ext_hom({_, _, _, _} = P)    -> P;
to_ext_hom(<<_:32/binary>> = P) -> decompress(P);
to_ext_hom({X, Y})              -> {X, Y, 1, ?MUL(X, Y)}.

to_affine({_, _} = P) -> P;
to_affine(<<_:32/binary>> = P) -> to_affine(decompress(P));
to_affine({X, Y, Z, _}) ->
  ZInv = f_inv(Z),
  {?MUL(X, ZInv), ?MUL(Y, ZInv)}.

f_pow(A, B) -> ecu_misc:exp_mod(A, B, ?P).

%% Arithmetics in prime field P
f_add(A, B) -> (A + B) rem ?P.
f_mul(A, B) -> (A * B) rem ?P.
f_sub(A, B) -> (A - B + ?P) rem ?P.
f_div(A, B) -> f_mul(A, f_inv(B)).

f_inv(A) ->
  f_pow(A, ?P - 2).

%% Arithmetics in curve group order N
s_add(<<A:256/little>>, <<B:256/little>>) -> <<((A + B) rem ?N):256/little>>.
s_mul(<<A:256/little>>, <<B:256/little>>) -> <<((A * B) rem ?N):256/little>>.
s_sub(<<A:256/little>>, <<B:256/little>>) -> <<((A - B + ?N) rem ?N):256/little>>.
s_div(A, B) -> s_mul(A, s_inv(B)).

s_inv(<<A:256/little>>) ->
  {1, S, _T} = ecu_misc:eea(A, ?N),
  <<((S + ?N) rem ?N):256/little>>.

scalar_reduce(<<S:512/little>>) ->
  <<(S rem ?N):256/little>>.

%% --- internal functions
scalar_mul_(0, _P) -> {0, 1, 1, 0};
scalar_mul_(1, P)  -> P;
scalar_mul_(K, P) ->
  case K rem 2 of
    0 -> scalar_mul_(K div 2, p_dbl(P));
    1 -> p_add(P, scalar_mul_(K - 1, P))
  end.

clamp(K) ->
  ((K band (bnot 7)) band (bnot (128 bsl 248))) bor (64 bsl 248).

xrecover(Y) ->
  Y2    = ?MUL(Y, Y),
  U     = ?SUB(Y2, 1),
  V     = ?ADD(?MUL(?D, Y2), 1),
  UdivV = ?DIV(U, V),
  X0    = f_pow(UdivV, ?PPLUS3DIV8),

  X0_2 = ?MUL(X0, X0),
  case ?SUB(X0_2, UdivV) of
    0 -> X0;
    _ -> case ?ADD(X0_2, UdivV) of
           0 -> ?MUL(X0, ?TWOPOWPMINUS1DIV4);
           _ -> error(xrecover_failed)
         end
  end.
