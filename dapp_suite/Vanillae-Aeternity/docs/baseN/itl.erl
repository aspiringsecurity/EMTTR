-module(itl).

-compile([export_all, nowarn_export_all]).

%% Base 1 makes no sense
digits(N, Base) when N > 0, Base > 1 ->
    digits(N, Base, []);
%% have to handle 0 specially because digits/3 will return [] in this case
digits(0, _Base) ->
    [0].


%% general case
digits(N, Base, DigitsAcc) when N > 0 ->
    Q            = N div Base,
    R            = N rem Base,
    NewN         = Q,
    NewDigitsAcc = [R | DigitsAcc],
    digits(NewN, Base, NewDigitsAcc);
%% terminate when N = 0
digits(0, _Base, Digits) ->
    Digits.
