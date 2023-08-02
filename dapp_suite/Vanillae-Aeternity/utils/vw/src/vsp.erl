% Seed phrases
% See: https://en.bitcoin.it/wiki/BIP_0039#Reference_Implementation

-module(vsp).

-compile([export_all, nowarn_export_all]).


%-spec encode(SecretKey) -> SeedPhrase
%    when SecretKey  :: <<_:128>>,
%         SeedPhrase :: [string()].
%
%encode(SecretKey) ->
%    <<CheckBytes:4/binary, _/binary>> = crypto:hash(sha256, Key).
