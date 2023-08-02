-module(vb64).

-compile([export_all]).
-export([enc/1, dec/1]).

-export([test/0]).

test() ->
    test(100).

test(N) when N >= 0 ->
    RandBytes = rand:bytes(N),
    % encode the bytes
    CorrectEnc = erlang:binary_to_list(base64:encode(RandBytes)),
    MyEnc      = enc(RandBytes),
    EncodeOk   = CorrectEnc =:= MyEnc,
    % decode the bytes
    MyDec      = dec(MyEnc),
    DecodeOk   = MyDec =:= RandBytes,
    % print results if error
    ok =
        case EncodeOk of
            true ->
                ok;
            false ->
                ok = io:format("===~n"
                               "Encode failure!~n"
                               "input           : ~tw~n"
                               "expected output : ~tw~n"
                               "actual output   : ~tw~n"
                               "===~n~n",
                               [RandBytes, CorrectEnc, MyEnc]),
                ok
        end,
    ok =
        case DecodeOk of
            true ->
                ok;
            false ->
                ok = io:format("===~n"
                               "Decode failure!~n"
                               "input           : ~tw~n"
                               "expected output : ~tw~n"
                               "actual output   : ~tw~n"
                               "===~n~n",
                               [MyEnc, RandBytes, MyDec]),
                ok
        end,
    % recurse
    test(N - 1);
test(_) ->
    ok.



-spec enc(Binary) -> Base64
    when Binary :: binary(),
         Base64 :: string().
%% @doc
%% "Encode" (from the perspective of the program) binary data into base64
%% @end


% general case: at least 3 bytes (24 bits = 6+6+6+6) remaining
%
% 12345678 abcdefgh 12345678    ...
% 123456 78abcd efgh12 345678   ...
%   A      B     C      D       Rest
% convert to chars ->
%   CA    CB     CC    CD
enc(<<A:6, B:6, C:6, D:6, Rest/binary>>) ->
    CA = int2char(A),
    CB = int2char(B),
    CC = int2char(C),
    CD = int2char(D),
    [CA, CB, CC, CD | enc(Rest)];
% terminal case: 2 bytes (16 bits = 6+6+4) remaining
%
% 12345678 abcdefgh
% 123456 78abcd   efgh__
%    A     B     C bsl 2
% convert to chars ->
%   CA     CB       CC    =
enc(<<A:6, B:6, C:4>>) ->
    CA = int2char(A),
    CB = int2char(B),
    CC = int2char(C bsl 2),
    [CA, CB, CC, $=];
% terminal case: 1 byte (8 bits = 6+2) remaining
%
% 12345678 ->
% 123456   78____
%    A     B bsl 4
% convert to chars ->
%   CA      CB     =    =
enc(<<A:6, B:2>>) ->
    CA = int2char(A),
    CB = int2char(B bsl 4),
    [CA, CB, $=, $=];
% terminal case: 0 bytes remaining
enc(<<>>) ->
    [].



-spec dec(Base64) -> Binary
    when Base64 :: string(),
         Binary :: binary().
%% @doc
%% "Decode" (from the perspective of the program) a Base64 string into binary data

dec(Base64_String) ->
    dec(Base64_String, <<>>).


% terminal case: two equal signs at the end = 1 byte (8 bits = 6+2) remaining
% input (characters) ->
%         W       X     =    =
% convert to numbers ->
%       abcdef gh____   =    =
%         NW      NX
% regroup ->
%          abcdefgh  ____        abcdef   gh____
%       <<LastByte:8, 0:4>> = <<  NW:6,    NX:6   >>
dec([W, X, $=, $=], Acc) ->
    NW = char2int(W),
    NX = char2int(X),
    <<LastByte:8, 0:4>> = <<NW:6, NX:6>>,
    <<Acc/binary, LastByte:8>>;
% terminal case: one equal sign at the end = 2 bytes remaining
%
% input (characters) ->
%         W       X     Y    =
% convert to numbers ->
%       abcdef gh1234  5678__  =
%         NW      NX    NY
% regroup ->
%          abcdefgh  12345678   __          abcdef    gh1234   5678__
%       <<   B1:8,     B2:8,   0:2  >> = <<  NW:6,     NX:6     NY:6   >>
dec([W, X, Y, $=], Acc) ->
    NW = char2int(W),
    NX = char2int(X),
    NY = char2int(Y),
    <<B1:8, B2:8, 0:2>> = <<NW:6, NX:6, NY:6>>,
    <<Acc/binary, B1:8, B2:8>>;
% terminal case: 0 bytes remaining
% nothing to do
dec([], Acc) ->
    Acc;
% general case: no equal signs = 3 or more bytes remaining
%
% input (characters) ->
%         W       X      Y      Z
% convert to numbers ->
%       abcdef gh1234  5678ab cdefgh
%         NW      NX    NY      NZ
% decompose ->
%          abcdefgh  12345678   abcdefgh          abcdef    gh1234   5678ab   cdefgh
%       <<   B1:8,     B2:8,      B3:2   >> = <<  NW:6,     NX:6     NY:6,    NZ:6   >>
dec([W, X, Y, Z | Rest], Acc) ->
    NW = char2int(W),
    NX = char2int(X),
    NY = char2int(Y),
    NZ = char2int(Z),
    NewAcc = <<Acc/binary, NW:6, NX:6, NY:6, NZ:6>>,
    dec(Rest, NewAcc).



int2char( 0) -> $A;
int2char( 1) -> $B;
int2char( 2) -> $C;
int2char( 3) -> $D;
int2char( 4) -> $E;
int2char( 5) -> $F;
int2char( 6) -> $G;
int2char( 7) -> $H;
int2char( 8) -> $I;
int2char( 9) -> $J;
int2char(10) -> $K;
int2char(11) -> $L;
int2char(12) -> $M;
int2char(13) -> $N;
int2char(14) -> $O;
int2char(15) -> $P;
int2char(16) -> $Q;
int2char(17) -> $R;
int2char(18) -> $S;
int2char(19) -> $T;
int2char(20) -> $U;
int2char(21) -> $V;
int2char(22) -> $W;
int2char(23) -> $X;
int2char(24) -> $Y;
int2char(25) -> $Z;
int2char(26) -> $a;
int2char(27) -> $b;
int2char(28) -> $c;
int2char(29) -> $d;
int2char(30) -> $e;
int2char(31) -> $f;
int2char(32) -> $g;
int2char(33) -> $h;
int2char(34) -> $i;
int2char(35) -> $j;
int2char(36) -> $k;
int2char(37) -> $l;
int2char(38) -> $m;
int2char(39) -> $n;
int2char(40) -> $o;
int2char(41) -> $p;
int2char(42) -> $q;
int2char(43) -> $r;
int2char(44) -> $s;
int2char(45) -> $t;
int2char(46) -> $u;
int2char(47) -> $v;
int2char(48) -> $w;
int2char(49) -> $x;
int2char(50) -> $y;
int2char(51) -> $z;
int2char(52) -> $0;
int2char(53) -> $1;
int2char(54) -> $2;
int2char(55) -> $3;
int2char(56) -> $4;
int2char(57) -> $5;
int2char(58) -> $6;
int2char(59) -> $7;
int2char(60) -> $8;
int2char(61) -> $9;
int2char(62) -> $+;
int2char(63) -> $/.


char2int($A) ->  0;
char2int($B) ->  1;
char2int($C) ->  2;
char2int($D) ->  3;
char2int($E) ->  4;
char2int($F) ->  5;
char2int($G) ->  6;
char2int($H) ->  7;
char2int($I) ->  8;
char2int($J) ->  9;
char2int($K) -> 10;
char2int($L) -> 11;
char2int($M) -> 12;
char2int($N) -> 13;
char2int($O) -> 14;
char2int($P) -> 15;
char2int($Q) -> 16;
char2int($R) -> 17;
char2int($S) -> 18;
char2int($T) -> 19;
char2int($U) -> 20;
char2int($V) -> 21;
char2int($W) -> 22;
char2int($X) -> 23;
char2int($Y) -> 24;
char2int($Z) -> 25;
char2int($a) -> 26;
char2int($b) -> 27;
char2int($c) -> 28;
char2int($d) -> 29;
char2int($e) -> 30;
char2int($f) -> 31;
char2int($g) -> 32;
char2int($h) -> 33;
char2int($i) -> 34;
char2int($j) -> 35;
char2int($k) -> 36;
char2int($l) -> 37;
char2int($m) -> 38;
char2int($n) -> 39;
char2int($o) -> 40;
char2int($p) -> 41;
char2int($q) -> 42;
char2int($r) -> 43;
char2int($s) -> 44;
char2int($t) -> 45;
char2int($u) -> 46;
char2int($v) -> 47;
char2int($w) -> 48;
char2int($x) -> 49;
char2int($y) -> 50;
char2int($z) -> 51;
char2int($0) -> 52;
char2int($1) -> 53;
char2int($2) -> 54;
char2int($3) -> 55;
char2int($4) -> 56;
char2int($5) -> 57;
char2int($6) -> 58;
char2int($7) -> 59;
char2int($8) -> 60;
char2int($9) -> 61;
char2int($+) -> 62;
char2int($/) -> 63.
