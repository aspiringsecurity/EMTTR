%%% File        : ecu_misc.erl
%%% Author      : Hans Svensson
%%% Description : Misc. functionality
%%% Created     : 13 Jan 2022 by Hans Svensson
-module(ecu_misc).

-export([eea/2, exp_mod/3,
         hex_to_bin/1, bin_to_hex/1,
         pcomp/1]).

%% A^B mod P
exp_mod(_A, 0, _P) -> 1;
exp_mod(A, B, P) when A > 0 ->
  binary:decode_unsigned(crypto:mod_pow(A, B, P));
exp_mod(A, B, P) ->
  X = exp_mod(-A, B, P),
  case B rem 2 == 0 orelse X == 0 of
    true  -> X;
    false -> P - X
  end.

%% Extended Euclidean Algorithm
eea(A, B) when ((A < 1) or (B < 1)) ->
    undefined;
eea(A, B) ->
    eea(A, 1, 0, B, 0, 1).

eea(G, S, T, 0, _, _) ->
    {G, S, T};
eea(G0, S0, T0, G1, S1, T1) ->
    Q = G0 div G1,
    eea(G1, S1, T1, G0 - (Q * G1), S0 - (Q * S1), T0 - (Q * T1)).

%% Very rudimentary parallel computation...
pcomp(Fs) ->
  Parent = self(),
  Pids = [ spawn(fun() -> Parent ! {self(), F()} end) || F <- Fs ],
  [ receive {Pid, X} -> X after 500 -> error(timeout) end || Pid <- Pids ].

%% Hex encode/decode
-spec hex_to_bin(Input :: string()) -> binary().
hex_to_bin(S) ->
  hex_to_bin(S, []).
hex_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hex_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hex_to_bin(T, [V | Acc]).

-spec bin_to_hex(Input :: binary()) -> string().
bin_to_hex(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).
