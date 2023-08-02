# Keccak, SHA-3, and SHAKE-N algorithms explained, with code in Erlang

![I can't make a diagram for this one, fellas.[^sponge]](./sponge.jpg)

## Contents

-   [Introduction](#introduction)
-   [tldr](#tldr)
    -   [References](#references)
    -   [Pitfalls](#pitfalls)
        -   [Pitfall: This is only keccak-f-1600](#pitfall-this-is-only-keccak-f-1600)
        -   [Pitfall: "fast keccak" versus "clear keccak"](#pitfall-fast-keccak-versus-clear-keccak)
        -   [Pitfall: "NIST SHA-3" versus "Keccak SHA-3"](#pitfall-nist-sha3-versus-keccak-sha-3)
        -   [Pitfall: SHAKE-N versus SHA3-N](#pitfall-shake-n-versus-sha3-n)
        -   [Pitfall: Greek letter steps require two copies of the sponge to compute](#pitfall-greek-letter-steps-require-two-copies-of-the-sponge-to-compute)
        -   [Pitfall: iota step round constant table](#pitfall-iota-step-round-constant-table)
-   [SHA-s and SHAKE-s](#sha-s-and-shake-s)
-   [Outer Keccak](#outer-keccak)
    -   [Outer Keccak: Padding](#outer-keccak-padding)
    -   [Outer Keccak: Absorption phase](#outer-keccak-absorption-phase)
    -   [Outer Keccak: Squeezing phase](#outer-keccak-squeezing-phase)
-   [Inner Keccak: the kek operation](#inner-keccak-the-kek-operation)
    -   [Inner Keccak: theta stage](#inner-keccak-theta-stage)
    -   [Inner Keccak: rho stage](#inner-keccak-rho-stage)
    -   [Inner Keccak: pi stage](#inner-keccak-pi-stage)
    -   [Inner Keccak: chi stage](#inner-keccak-chi-stage)
    -   [Inner Keccak: iota stage](#inner-keccak-iota-stage)
    -   [Inner Keccak: Coordinate System](#inner-keccak-coordinate-system)
        -   [Inner Keccak Coordinate System: Vocabulary](#inner-keccak-coordinate-system-vocabulary)
            - [3D state](#3d-state)
            - [0D subsets of the state](#0d-subsets-of-the-state)
            - [1D subsets of the state](#1d-subsets-of-the-state)
            - [2D subsets of the state](#2d-subsets-of-the-state)
        -   [Inner Keccak Coordinate System: Code](#inner-keccak-coordinate-system-code)
-   [Conclusion](#conclusion)


## Introduction

Keccak is a hashing algorithm used for the SHA-3 standard. [The NIST
standard][nist-standard] is semi-readable math clownery. Hashing algorithms by
nature have to be complicated and somewhat obfuscated, so this is kind of par
for the course.

I found [this lecture][german-lecture] and the [accompanying
notes][german-lecture-notes] indispensable when writing kek.

At the end of the day, all Keccak does is take in some input bits, process them
in a deterministic way, and hand you output bits.

The goal of this document is just to explain what the processing steps are as
straightforwardly as possible.

Keccak is the "general case", and then SHA-3 and SHAKE-128 and so on are
special cases of Keccak.

Hans Svensson and Craig Everett made extremely valuable contributions to this
project.

## tldr

- [Erlang code (clear)](https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl)
- [Erlang code (fast)](https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek_fast.erl)
- [erlang-sha3 library (uses fast version of kek)](https://github.com/zxq9/erlang-sha3/blob/63193654e3c05d8031300ffcd52092f75e8b5c2f/src/sha3.erl#L85-L112)
- [iota step round constant computing code][rc-erl]

### References

1. [Helpful lecture][german-lecture]
2. [Notes for that lecture][german-lecture-notes]
3. [NIST standard][nist-standard] (btw: the double bar notation means "concatenate")
4. [SHA-3 Wikipedia](https://en.wikipedia.org/wiki/SHA-3)

### Pitfalls

#### Pitfall: This is only keccak-f-1600

Keccak has an "outer keccak" and an "inner keccak".  The standard specifies
this free parameter called `b`, which is the bit size of the algorithm's state.
For what I needed, this can be assumed to be `1600`.  You may need to
generalize this code.  Hopefully this explainer is clear enough that you can do
that.

![[NIST standard][nist-standard], pp. 17](./keccak-f.png)


#### Pitfall: "fast keccak" versus "clear keccak"

The main reference is the "clear" Erlang code.  The "fast" version is the
"clear" version with optimizations applied.  The fast version was written by
Hans Svensson.

In particular, fast keccak is arity 4, and clear keccak is arity 3.

There are a couple of trivial differences in code structure, so when you're
reading code, you need to keep in mind which version you're looking at. When I
have a code sample, I will link the GitHub permalink in a comment where the
code originates from, so there shouldn't be any confusion.

```erlang
%% From "clear keccak"
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L155-L159
-spec keccak(Capacity, Message, OutputBitLength) -> Digest
    when Capacity        :: pos_integer(),
         Message         :: bitstring(),
         OutputBitLength :: pos_integer(),
         Digest          :: bitstring().

%% From "fast Keccak"
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek_fast.erl#L142-L147
-spec keccak(Capacity, Message, Delimiter, OutputBitLength) -> Digest
    when Capacity        :: pos_integer(),
         Message         :: bitstring(),
         Delimiter       :: bitstring(),
         OutputBitLength :: pos_integer(),
         Digest          :: bitstring().
```

The reason for this is one of Hans's optimizations was speeding up the manner
in which padding was applied, and that required factoring it out into an
argument.

#### Pitfall: "NIST SHA-3" versus "Keccak SHA-3"

There are two versions of "SHA-3".  There's the "NIST version" and the "keccak
version".  The only difference is that NIST adds two bits (yes bits, not bytes)
of padding

```erlang
%% From the erlang-sha3 lib, which uses the fast version
%% - ignore the capacity and what not for now, will be explained in a minute
%% - OutputBitLength is the N in SHA3-N. So it would be 512 for SHA3-512, 256
%%   for SHA3-256, etc.
%% - Message is the input bitstring to be hashed

%% https://github.com/zxq9/erlang-sha3/blob/63193654e3c05d8031300ffcd52092f75e8b5c2f/src/sha3.erl#L107-L112
kek(OutputBitLength, Message, keccak) ->
    Capacity = 2 * OutputBitLength,
    keccak(Capacity, Message, <<>>, OutputBitLength);
                          %%  ^ keccak sha3 adds no padding bits
kek(OutputBitLength, Message, nist) ->
    Capacity = 2 * OutputBitLength,
    keccak(Capacity, Message, <<2#01:2>>, OutputBitLength).
                          %%  ^ nist sha3 concats these padding bits to the end of Message
```

The reason this problem exists is the "keccak version" was used in production
code before the NIST standard was published. So now there are two versions
of "SHA-3" out there.

#### Pitfall: SHAKE-N versus SHA3-N

Note that the padding and sha3-is-keccak-but-its-not pitfall only applies to
the SHA-3 fixed-length hashing functions. These produce a fixed-length (say,
512 bit) hash.

The SHAKE-N algorithms are sort of "arbitrary length SHA-3". So instead of
producing 512 bits, you tell it how long the output length should be.  The
SHAKE-N algorithms have their own padding rule, and I don't believe they're
affected by the padding thing.

```erlang
%% From the "clear" version
%% The clear version was written against the NIST standard

%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L86-L89
sha3(OutputBitLength, Message) ->
    Capacity = 2*OutputBitLength,
    %% SHA-3 NIST padding is used in the clear version
    ShaMessage = <<Message/bitstring, (2#01):2>>,
    keccak(Capacity, ShaMessage, OutputBitLength).

%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L131-L134
shake(ShakeNumber, Message, OutputBitLength) ->
    Capacity = 2*ShakeNumber,
    %% SHAKE has its own padding rule
    ShakeMessage = <<Message/bitstring, (2#1111):4>>,
    keccak(Capacity, ShakeMessage, OutputBitLength).
```

#### Pitfall: Greek letter steps require two copies of the sponge to compute

This is the pitfall I ran into, and it stalled this project for months. I
couldn't figure out what the issue was, and the project was only saved because
Hans took the time to figure out the issue and fix it. The nature of hashing
algorithms is that they are all-or-nothing and are hard to decompose.  Usually
when your code doesn't work, you can do some trial and error to figure out
which part of the code is wrong.  Not so with hashing.

This algorithm has state, which is a 1600-bit bit array called the "sponge".
And that state gets updated a lot.  The updating procedure involves a lot of
`xor`ing this bit against that bit, and computing the parity of this string of
bits, etc.

A lot of it involves crawling down the bit array one bit at a time, and xoring
the current bit against certain bits from **the original bit array** (er...
"original" within the context of that Greek letter step... read the code), and
then keeping a separate copy that has the modifications.

The mistake I was making was as follows. Let's say that the update to bit 55
requires xoring it against bit 30. It generally requires xoring against the
**un-updated** bit 30.  I didn't catch that detail, and was xoring against the
adulterated bit 30.

There's simply no way to find this mistake from trial and error.  It's too
subtle.

You can see the commit where Hans fixed my mistake
[here](https://github.com/pharpend/kek/commit/7d67c40e6e1280f4abd4fce9122a71034ebcc142).


#### Pitfall: iota step round constant table

Do not blindly copy the round constant table from the [iota
step](#inner-keccak-iota-stage).  The iota step involves xoring some bits from
the sponge with one of 24 constant bit arrays, called "round constants"... it
will make sense when you get there.

The [NIST standard][nist-standard][^alg5] specifies a *procedure* for
calculating the constant bit arrays...  again, this is because the NIST
standard is [general over lane length](#pitfall-this-is-only-keccak-f-1600),
and our code is not.

The bit-endianness of the constants depends on how you have structured your
constant-computing code *and* your sponge-bit-fetching code.  So if you look in
two different sources, you are likely to find two different sets of constants.

[Here][rc-erl] is how I computed my answer.  That code existed to check that a
table I found[^tbl] was correct.  It turns out that table has bit-endianness
opposite from what made sense for my code.

## SHA-s and SHAKE-s

These are the "porcelain" functions that we show to the outside world.  These
are just rewrites in front of Keccak.  Another way to think about it is that
Keccak has tons of settings, and each of these "algorithms" are just different
settings presets.

```erlang
%% From: https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L26-L134

-spec sha3_224(Message) -> Digest
    when Message :: bitstring(),
         Digest  :: <<_:224>>.
%% @doc
%% SHA-3 with an output bit length of 224 bits.
%% @end

sha3_224(Message) ->
    sha3(224, Message).



-spec sha3_256(Message) -> Digest
    when Message :: bitstring(),
         Digest  :: <<_:256>>.
%% @doc
%% SHA-3 with an output bit length of 256 bits.
%% @end

sha3_256(Message) ->
    sha3(256, Message).



-spec sha3_384(Message) -> Digest
    when Message :: bitstring(),
         Digest  :: <<_:384>>.
%% @doc
%% SHA-3 with an output bit length of 384 bits.
%% @end

sha3_384(Message) ->
    sha3(384, Message).



-spec sha3_512(Message) -> Digest
    when Message :: bitstring(),
         Digest  :: <<_:512>>.
%% @doc
%% SHA-3 with an output bit length of 512 bits.
%% @end

sha3_512(Message) ->
    sha3(512, Message).



-spec sha3(OutputBitLength, Message) -> Digest
    when OutputBitLength :: pos_integer(),
         Message         :: bitstring(),
         Digest          :: bitstring().
%% @doc
%% SHA-3 with an arbitrary output bit length.
%%
%% This means Keccak with Capacity = 2*OutputBitLength. Additionally, SHA3
%% concatenates the bits 01 onto the end of the input, before sending the
%% Message to keccak/3.
%% @end

sha3(OutputBitLength, Message) ->
    Capacity = 2*OutputBitLength,
    ShaMessage = <<Message/bitstring, (2#01):2>>,
    keccak(Capacity, ShaMessage, OutputBitLength).



-spec shake128(Message, OutputBitLength) -> Digest
    when Message         :: bitstring(),
         OutputBitLength :: pos_integer(),
         Digest          :: bitstring().
%% @doc
%% This is the SHAKE variable-length hash with Capacity 256 = 2*128 bits.
%% @end

shake128(Message, OutputBitLength) ->
    shake(128, Message, OutputBitLength).



-spec shake256(Message, OutputBitLength) -> Digest
    when Message         :: bitstring(),
         OutputBitLength :: pos_integer(),
         Digest          :: bitstring().
%% @doc
%% This is the SHAKE variable-length hash with Capacity 512 = 2*256 bits.
%% @end

shake256(Message, OutputBitLength) ->
    shake(256, Message, OutputBitLength).



-spec shake(ShakeNumber, Message, OutputBitLength) -> Digest
    when ShakeNumber     :: pos_integer(),
         Message         :: bitstring(),
         OutputBitLength :: pos_integer(),
         Digest          :: bitstring().
%% @doc
%% This is the SHAKE variable-length hash with Capacity 512 = 2*ShakeNumber bits.
%%
%% This concatenates the bitstring 1111 onto the end of the Message before
%% sending the message to keccak/3.
%% @end

shake(ShakeNumber, Message, OutputBitLength) ->
    Capacity = 2*ShakeNumber,
    ShakeMessage = <<Message/bitstring, (2#1111):4>>,
    keccak(Capacity, ShakeMessage, OutputBitLength).
```

## Outer Keccak

Keccak uses this "sponge" metaphor.  If you think of the bits as water, the
state type of Keccak is somewhat analagous to a sponge. In this analogy, the
Keccak algorithm consists of "absorbing" the bits into the sponge, and then
"squeezing" the bits out.

Both the absorption and squeezing phases invoke "inner keccak", which is where
all the real bit-churning happens. (These are the Greek letter steps in the
standard).

`inner_keccak/1` is a 1-arity function, and the input is just the sponge. It
scrambles the bits and does some `xor`ing and what not.  The operation of
applying `inner_keccak/1` to a sponge to get a new sponge is called
**kekking**.  This is vocabulary I invented and have found useful (you will not
find this in the official docs).

Just pay attention to the flow for now.

- We take in our input (`Message`).
- Add some padding bits to get `PaddedMessage`.
- `InitialSponge` is a 1600-bit long array of `0` bits (the "dry sponge").
- We make the dry `InitialSponge` wet by `absorb/4`ing the `PaddedMessage`, to create `WetSponge`
- We `squeeze/3` the `WetSponge` out to get the `ResultBits`

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L155-L172

-spec keccak(Capacity, Message, OutputBitLength) -> Digest
    when Capacity        :: pos_integer(),
         Message         :: bitstring(),
         OutputBitLength :: pos_integer(),
         Digest          :: bitstring().
%% @doc
%% Note: this is Keccak 1600, the only one used in practice
%%
%% Capacity must be strictly less than 1600
%% @end

keccak(Capacity = _c, Message, OutputBitLength) ->
    BitRate       = 1600 - Capacity,
    PaddedMessage = pad(Message, BitRate),
    InitialSponge = <<0:1600>>,
    WetSponge     = absorb(PaddedMessage, BitRate, Capacity, InitialSponge),
    ResultBits    = squeeze(WetSponge, OutputBitLength, BitRate),
    ResultBits.
```

The padding part is kind of dumb. `absorb/4` and `squeeze/3` both call
`inner_keccak/1`, which like I said is where all the real bit-churning happens.

### Outer Keccak: Padding

The absorption phase is "chunked", meaning

1. it consumes a chunk of the input bits
2. updates the state (the "sponge")
3. if there are no more input bits to consume, absorption is done
4. else, go to step (1)

The size of this chunk is called the `BitRate`. In order for this to work, the
bit length of the input has to be a multiple of `BitRate`. The padding step
exists to make sure that is true.

The padding rule is `/10*1/`, as in the regex. So for instance, suppose our bit
rate was `10`.

```
input (bytes)           : 00000111 10101100 01001111
input (grouped into 10) : 0000011110 1011000100 1111______
padded input            : 0000011110 1011000100 1111100001
```

The rules are

1. There will *always* be at least two bits of padding added to the end of the
   bitstring
2. The first padding bit will be `1`
3. The last padding bit will be `1`
4. *If* there are intermediate padding bits, they will all be `0`
5. The total number of padding bits added is however many makes the length of
   the resulting bitstring an integer multiple of the `BitRate`

With that in mind:

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L176-L234

-spec pad(Message, BitRate) -> NewMessage
    when Message    :: bitstring(),
         BitRate    :: pos_integer(),
         NewMessage :: bitstring().
%% @private
%% padding
%% divide the message into r-bit blocks
%%
%% the message ends with 1000...0001
%%
%% sha3 calls this /10*1/ as in the regex
%%
%% Reference: https://en.wikipedia.org/wiki/SHA-3#Padding
%% @end

% note: rem will always return a positive integer because both bit_size
% case when the message bit length is evenly divisible by the bit rate
% in this case we add a whole new r-word
pad(Message, BitRate = _r) when (bit_size(Message) rem BitRate) =:= 0 ->
    % Suppose the BitRate was 8 and we had 0 bits left
    % Input:
    %   Bits: <<>>
    %   Idx1: 12345678
    % Result:
    %   Bits: 10000001
    %   Idx1: 12345678
    % In this case we add a new r-word
    NewRWord   = <<1:1, 0:(BitRate - 2), 1:1>>,
    NewMessage = <<Message/bitstring, NewRWord/bitstring>>,
    NewMessage;
% this is the retarded case, when the bit length of the message is exactly one
% bit less than dividing the BitRate
pad(Message, BitRate = _r) when (bit_size(Message) rem BitRate) =:= (BitRate - 1) ->
    % Suppose the BitRate was 8 and we had 7 bits left
    % Input:
    %   Bits: ABCDEFG
    %   Idx1: 12345678
    % Result:
    %   Bits: ABCDEFG1 00000001
    %   Idx1: 12345678 12345678
    % in this case, we add a 1, (r-1) zeros, and a trailing 1
    NewRWord   = <<1:1, 0:(BitRate - 1), 1:1>>,
    NewMessage = <<Message/bitstring, NewRWord/bitstring>>,
    NewMessage;
% this is the general case, where there are at least 2 bits left in order to
% fill out the r-word
pad(Message, BitRate = _r) ->
    % Suppose the BitRate was 8 and we had 3 bits left
    % Input:
    %   Bits: ABC
    %   Idx1: 12345678
    % Result:
    %   Bits: ABC10001
    %   Idx1: 12345678
    NumberOfMessageBitsInTheLastRWord = bit_size(Message) rem BitRate,
    NumberOfNewBitsNeeded             = BitRate - NumberOfMessageBitsInTheLastRWord,
    NumberOfNewZerosNeeded            = NumberOfNewBitsNeeded - 2,
    NewMessage                        = <<Message/bitstring, 1:1, 0:NumberOfNewZerosNeeded, 1:1>>,
    NewMessage.
```

### Outer Keccak: Absorption phase

As I mentioned above, absorption is "chunked", and the chunk size is the bit
rate.  Absorption comes after padding.  So in our absorb procedure, we can
*assume* the length of the `PaddedMessage` is an integer multiple of `BitRate`.

The procedure is

1.  Consume `BitRate` bits off the input `PaddedMessage` and put these bits
    into `ThisRWord`
2.  Bitwise xor `ThisRWord` against the `Sponge`

    Let's suppose `BitRate = 10` as before

    ```
    PaddedMessage : 0000011110 1011000100 1111100001
    ThisRWord     : 0000011110
    Sponge        : 1111110000 0100011010 1001111010 ... (1600 bits)
    AugRWord      : 0000011110 0000000000 0000000000 ... (1600 bits)
    InnerKekInput : 1111101110 0100011010 1001111010 ... (1600 bits, result of xoring the two previous lines)
    ```

3.  Take the freshly xored sponge and kek it
4.  Repeat until you run out of `PaddedMessage` bits.


```erlang
-spec absorb(PaddedMessage, BitRate, Capacity, SpongeAcc) -> WetSponge
    when PaddedMessage :: bitstring(),
         BitRate       :: pos_integer(),
         Capacity      :: pos_integer(),
         SpongeAcc     :: <<_:1600>>,
         WetSponge     :: <<_:1600>>.
%% @private
%% Assumptions:
%%  1. BitRate + Capacity = 1600,
%%  2. BitRate divides the PaddedMessage length (i.e. already have done padding)
%% @end

% can pull off r bits from the start of the message
absorb(PaddedMessageBits, BitRate = _r, Capacity = _c, Sponge) when BitRate =< bit_size(PaddedMessageBits) ->
    <<ThisRWord:BitRate, Rest/bitstring>> = PaddedMessageBits,
    % we bitwise xor the sponge against the r word followed by a bunch of 0s
    <<SpongeInt:1600>> = Sponge,
    <<Foo:1600>>       = <<ThisRWord:BitRate, 0:Capacity>>,
    FInputInt          = SpongeInt bxor Foo,
    FInputBits         = <<FInputInt:1600>>,
    NewSponge          = inner_keccak(FInputBits),
    absorb(Rest, BitRate, Capacity, NewSponge);
% empty string, return the sponge
absorb(<<>>, _r, _c, FinalSponge) ->
    FinalSponge.
```

### Outer Keccak: Squeezing phase

Again, the procedure is to "absorb" bits into the algorithm's state (the
"sponge"), and then "squeeze" them out.

The way we squeeze them out is the opposite of the absorption procedure

1. Grab `BitRate` bits off the front of the `WetSponge`
2. Concatenate them to our `ResultBits`
3. If we have enough `ResultBits`, return them
4. Otherwise, re-kek the `WetSponge` and try again.

There is some commented out code here which I'm going to leave in.  The reason
is it highlights that this procedure is trivial in the case of the SHA3-N
algorithms. For instance, SHA3-512 has a result length of 512 bits, and the
sponge is always 1600 bits long. So it is a 1-step procedure.

For instance:

```
OutputBitLength = 512
Capacity        = 1024 = 2*OutputBitLength
BitRate         = 576  = 1600 - Capacity
```

So in the SHA3-N cases, our "squeezing" phase really just amounts to grabbing N
bits off the front of the sponge.

The squeezing phase only requires the re-kekking in the case of the SHAKE-N
algorithms where the `OutputBitLength > BitRate`.  Even then, it only requires
iterative re-kekking if the user picks a very long `OutputBitLength` (remember
this is a free parameter in SHAKE-N).

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L266-L300

-spec squeeze(WetSponge, OutputBitLength, BitRate) -> ResultBits
    when WetSponge       :: <<_:1600>>,
         OutputBitLength :: pos_integer(),
         BitRate         :: pos_integer(),
         ResultBits      :: bitstring().
%% @private
%% squeeze the output bits out of the sponge
%% @end

%%% % simple case: bit length is less than (or equal to) the sponge size, just grab
%%% % the first ones
%%% % this is the case for the shas
%%% squeeze(<<ResultBits:OutputBitLength, _Rest/bitstring>>, OutputBitLength, _BitRate) ->
%%%     <<ResultBits:OutputBitLength>>;
% general case: output bit length is greater than the sponge size, construct
% accumulatively
% this is the case for the variable-length encodings
squeeze(WetSponge, OutputBitLength, BitRate) ->
    InitOutputAcc = <<>>,
    really_squeeze(WetSponge, OutputBitLength, BitRate, InitOutputAcc).

% terminal case: we have enough bits in the output, return those
really_squeeze(_WetSponge, OutputBitLength, _BitRate, FinalAccBits) when OutputBitLength =< bit_size(FinalAccBits) ->
    <<ResultBits:OutputBitLength, _/bitstring>> = FinalAccBits,
    <<ResultBits:OutputBitLength>>;
% general case: need moar bits
% in this case
%   - we grab the first r bits of the sponge, add them to the accumulator
%   - re-kek the sponge
%   - try again
really_squeeze(WetSponge, OutputBitLength, BitRate, ResultAcc)->
    <<ThisRWord:BitRate, _/bitstring>> = WetSponge,
    NewResultAcc                       = <<ResultAcc/bitstring, ThisRWord:BitRate>>,
    NewWetSponge                       = inner_keccak(WetSponge),
    really_squeeze(NewWetSponge, OutputBitLength, BitRate, NewResultAcc).
```

## Inner Keccak: the kek operation

This is the `f` function that you see in all the documentation.  In inner
keccak, the 1600-bit sponge is now thought of as a 5x5x64 3-dimensional array.

Go re-read the [Greek letter pitfall
section](#pitfall-greek-letter-steps-require-two-copies-of-the-sponge-to-compute)
before continuing.

![[NIST standard][nist-standard], page 11](./spongecoords.png)

- The input is the 1600-bit sponge.
- The output is a new 1600-bit sponge.
- The sponge is sent through 24 "rounds".
- Each round consists of the 5 greek letter steps.
- Each Greek letter which is a fairly straightforward transformation to the
  input bit array.
- The Greek letter steps are called theta, rho, pi, chi, and iota.
- The iota step depends on which round we are in (indexed `0..23`)

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L324-L368

-spec inner_keccak(Sponge) -> NewSponge
    when Sponge    :: <<_:1600>>,
         NewSponge :: <<_:1600>>.
%% @private
%% the "inner keccak" function, or the 'f' function
%% a bunch of bit bullshit
%% @end

inner_keccak(Sponge) ->
    rounds(Sponge, 24).



-spec rounds(Sponge, NumRoundsLeft) -> ResultSponge
    when Sponge        :: <<_:1600>>,
         NumRoundsLeft :: non_neg_integer(),
         ResultSponge  :: <<_:1600>>.
%% @private
%% do however many rounds
%% @end

% no rounds left
rounds(FinalSponge, 0) ->
    FinalSponge;
rounds(Sponge, NumRoundsLeft) ->
    % NRoundsLeft = 24
    % idx0 = 0
    % NRoundsLeft = 1
    % idx0 = 23
    RoundIdx0        = 24 - NumRoundsLeft,
    NewSponge        = rnd(RoundIdx0, Sponge),
    NewNumRoundsLeft = NumRoundsLeft - 1,
    rounds(NewSponge, NewNumRoundsLeft).



-spec rnd(RoundIdx0, Sponge) -> NewSponge
    when RoundIdx0 :: 0..23,
         Sponge    :: <<_:1600>>,
         NewSponge :: <<_:1600>>.
%% @private
%% do a single round
%% @private

rnd(RoundIdx0, Sponge) ->
    iota(RoundIdx0, chi(pi(rho(theta(Sponge))))).
```

The Greek letter steps will often reference "the column to the front" or some
such.  The details of this are described in the [coordinate system
section][coord-system].  For now, trust that the function `left/1` correctly
fetches the X-coordinate to our left (and so forth for `front/1`, `up/1`, etc),
and worry about the details later.

![[NIST standard][nist-standard], p.8](./spongeparts.png)

### Inner Keccak: Theta stage

Keep [the Greek letter pitfall][greek-letter-pitfall] in mind.

For each bit in the sponge:

1.  take

    - the bit
    - the 5-bit column to the left
    - the 5-bit column to the front right

    in **the original array** (beware the [Greek letter pitfall][greek-letter-pitfall]!)

2.  compute the parity of the concatenation; that is, in the concatenated
    bitstring

    - if the total number of `1`s is even, the parity is `0`
    - if the total number of `1`s is odd, the parity is `1`

3.  set the bit to that parity value from step (2)

![[NIST standard][nist-standard], pp. 12](./theta.png)

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L372-L444

-spec theta(Array) -> NewArray
    when Array    :: <<_:1600>>,
         NewArray :: <<_:1600>>.
%% @private
%% the theta step
%% go bit by bit, applying a weird transformation to each bit
%% @end

theta(Array) ->
    theta(Array, Array, 0).


-spec theta(Array, OldArray, Idx0) -> NewArray
    when Array    :: <<_:1600>>,
         OldArray :: <<_:1600>>,
         Idx0     :: 0..1599,
         NewArray :: <<_:1600>>.
%% @private
%% the theta step
%% go bit by bit, applying a weird transformation to each bit
%% @end

% done
theta(ResultArray, _, 1600) ->
    ResultArray;
% do the weird permutation
% x = left/right             -/+
% y = down/up                -/+
% z = outOfScreen/intoScreen -/+
%     front/behind           -/+
% left-handed coordinate system but what can you do
theta(ArrayBits, OrigArray, ThisIdx0) ->
    <<Before:ThisIdx0, ThisBit:1, Rest/bitstring>> = ArrayBits,
    {xyz, ThisX, _ThisY, ThisZ} = idx0_to_xyz(ThisIdx0),
    XToTheLeft                  = left(ThisX),
    XToTheRight                 = right(ThisX),
    ZToTheFront                 = front(ThisZ),
    ColumnToTheLeft             = xzth({xz, XToTheLeft, ThisZ}, OrigArray),
    ColumnToTheFrontRight       = xzth({xz, XToTheRight, ZToTheFront}, OrigArray),
    NewBit                      = parity(<<ColumnToTheLeft/bitstring, ColumnToTheFrontRight/bitstring, ThisBit:1>>),
    NewBits                     = <<Before:ThisIdx0, NewBit:1, Rest/bitstring>>,
    NewIdx0                     = ThisIdx0 + 1,
    theta(NewBits, OrigArray, NewIdx0).


-spec parity(Bits) -> Parity
    when Bits   :: bitstring(),
         Parity :: 0 | 1.
%% @private
%% Count the number of 1s in the given bitstring. Return 0 if even, 1 if odd.
%% @end

parity(Bits) ->
    parity(Bits, 0).

parity(<<0:1, Rest/bitstring>>, NOnes) -> parity(Rest, NOnes);
parity(<<1:1, Rest/bitstring>>, NOnes) -> parity(Rest, NOnes + 1);
parity(<<>>                   , NOnes) -> NOnes rem 2.
```

### Inner Keccak: rho stage

This stage iterates over the `{X, Y}`-coordinate pairs, and applies an affine
shift to the remaining `Z`-coordinate.

![[NIST standard][nist-standard], pp. 13](./rho.png)

The shifts are given by a table which is in a janky order because look how
smart we don't you get it it's modular arithmetic it means 4 and 3 are like
negative numbers I am so smart please tell me how smart I am

![[NIST standard][nist-standard], pp. 13](./rho-table-wojak.png)

The outer part of this code `rho/1` and `rho/2` just contains the loopy part.
The actual transformation is in `rhoxy/2`. Erlang doesn't have loops.  Instead
we pick a starting point...

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L459-L467

-spec rho(Array) -> NewArray
    when    Array :: <<_:1600>>,
         NewArray :: <<_:1600>>.
%% @private
%% do the rho step
%% @end

rho(Array) ->
    rho(Array, {xy, 0, 0}).
```

Then on each step, update the `{X, Y}` value, with our branching logic to test
if we're done...

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L471-L492

-spec rho(Array, LaneXY) -> NewArray
    when Array    :: <<_:1600>>,
         LaneXY   :: {xy, 0..4, 0..4},
         NewArray :: <<_:1600>>.
%% @private
%% do the rho step to each lane
%% @end

% terminal case
rho(Array, XY = {xy, 4, 4}) ->
    Result = rhoxy(Array, XY),
    Result;
% need to reset Y and increment X
rho(Array, XY = {xy, X, 4}) ->
    NewArray = rhoxy(Array, XY),
    NewXY    = {xy, X + 1, 0},
    rho(NewArray, NewXY);
% need to increment Y and leave X
rho(Array, XY = {xy, X, Y}) ->
    NewArray = rhoxy(Array, XY),
    NewXY    = {xy, X, Y + 1},
    rho(NewArray, NewXY).
```

The actual transformation looks up the offset for the `{X, Y}` coordinate in a
table and applies it. `xyset/3` is a function that replaces a lane.  This is
explained in the [coordinate system section.](#inner-keccak-coordinate-system).

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L496-L517

-spec rhoxy(Array1600, LaneXY) -> NewArray1600
    when Array1600    :: <<_:1600>>,
         LaneXY       :: {xy, 0..4, 0..4},
         NewArray1600 :: <<_:1600>>.
%% @private
%% do the rho step to a given lane
%% @end

rhoxy(Array, ThisXY = {xy, ThisX, ThisY}) ->
    ThisOffset = offset(ThisX, ThisY),
    ThisLane   = xyth(ThisXY, Array),
    % we increase the z coordinate by the offset
    % Suppose the offset is 2
    %      bits = A B C D E
    %         z = 0 1 2 3 4
    %   newbits = D E A B C
    % in other words, we take Offset number of bits off the tail of the lane
    % put them on the front
    <<Foo:(64 - ThisOffset), Bar:ThisOffset>> = ThisLane,
    NewLane  = <<Bar:ThisOffset, Foo:(64 - ThisOffset)>>,
    NewArray = xyset(ThisXY, Array, NewLane),
    NewArray.
```

The `offset/2` table is copied from the table on pp. 13 of the [NIST
specification][nist-standard].  The reason for the `rem 64` is that our lane
depth is `64` because [we're not implementing full generalized
keccak](#pitfall-this-is-only-keccak-f-1600), we're only implementing the
keccak that is actually used in SHA-3 and SHAKE.

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L521-L558

-spec offset(X, Y) -> Offset
    when X      :: 0..4,
         Y      :: 0..4,
         Offset :: 0..63.
%% @private
%% See NIST specification, pg. 21
%% @end

offset(3, 2) -> 153 rem 64;
offset(3, 1) ->  55 rem 64;
offset(3, 0) ->  28 rem 64;
offset(3, 4) -> 120 rem 64;
offset(3, 3) ->  21 rem 64;

offset(4, 2) -> 231 rem 64;
offset(4, 1) -> 276 rem 64;
offset(4, 0) ->  91 rem 64;
offset(4, 4) ->  78 rem 64;
offset(4, 3) -> 136 rem 64;


offset(0, 2) ->   3 rem 64;
offset(0, 1) ->  36 rem 64;
offset(0, 0) ->   0 rem 64;
offset(0, 4) -> 210 rem 64;
offset(0, 3) -> 105 rem 64;

offset(1, 2) ->  10 rem 64;
offset(1, 1) -> 300 rem 64;
offset(1, 0) ->   1 rem 64;
offset(1, 4) ->  66 rem 64;
offset(1, 3) ->  45 rem 64;

offset(2, 2) -> 171 rem 64;
offset(2, 1) ->   6 rem 64;
offset(2, 0) -> 190 rem 64;
offset(2, 4) -> 253 rem 64;
offset(2, 3) ->  15 rem 64.
```

### Inner Keccak: pi stage

The effect of this step is to rearrange the lanes

```
Result[X, Y] = Input[X + 3*Y, X]

(mod 5 of course)
```

![[NIST standard][nist-standard], pp. 14](./pi.png)

My code ("clear version") accomplishes this by

1.  Converting the 1600-bit flat sponge array into a hashmap from `{xy, X, Y}`
    coordinate pairs to 64-bit lanes (a "lane map")
2.  Construct a new lane map by folding over `{xy, X, Y}` coordinate pairs,
    and for each `{xy, NewX, NewY}`, grabbing `{xy, NewX + 3*NewY, NewX}` from
    the lane map constructed in step (1).
3.  Convert the lane map from step (2) back to a 1600-bit flat sponge array.

This is super inefficient.  The fast version is faster.  You are welcome to go
read that if you want it to go faster.


```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L568-L586

-spec pi(Array1600) -> NewArray1600
    when Array1600    :: <<_:1600>>,
         NewArray1600 :: <<_:1600>>.
%% @private
%% The effect of this step is to rearrange the lanes
%%
%% Result[X, Y] = Input[X + 3*Y, X]
%%
%% (mod 5 of course)
%% @end

pi(Array1600) ->
    % what I'm going to make is a map #{{xy, X, Y} := Lane}
    % then make a new map from the which applies the coordinate transformation
    % then convert it back into an array
    OriginalLaneMap = lane_map(Array1600, #{}, {xy, 0, 0}),
    NewLaneMap      = new_lane_map(OriginalLaneMap, #{}, {xy, 0, 0}),
    NewArray1600    = lane_map_to_arr1600(NewLaneMap, <<0:1600>>, {xy, 0, 0}),
    NewArray1600.
```

Constructing the lane map for step (1) is a fold that works exactly the way you
expect. The only potential point of weirdness is `xyth/2` which is explained in
the [coordinate system section](#inner-keccak-coordinate-system).

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L590-L617

-spec lane_map(Array1600, MapAcc, Coord) -> LaneMap
    when Array1600 :: <<_:1600>>,
         MapAcc    :: #{XY := Lane},
         Coord     :: XY,
         LaneMap   :: #{XY := Lane},
         XY        :: {xy, X :: 0..4, Y :: 0..4},
         Lane      :: <<_:64>>.
%% @private
%% Make a map #{XY := Lane}
%% @end

% terminal case, end of array
lane_map(Array1600, MapAcc, ThisXY = {xy, 4, 4}) ->
    ThisLane = xyth(ThisXY, Array1600),
    FinalMap = MapAcc#{ThisXY => ThisLane},
    FinalMap;
% end of Y value, set Y to 0 and increment X
lane_map(Array1600, MapAcc, ThisXY = {xy, X, 4}) ->
    ThisLane  = xyth(ThisXY, Array1600),
    NewMapAcc = MapAcc#{ThisXY => ThisLane},
    NewXY     = {xy, X + 1, 0},
    lane_map(Array1600, NewMapAcc, NewXY);
% general case: increment Y value
lane_map(Array1600, MapAcc, ThisXY = {xy, X, Y}) ->
    ThisLane  = xyth(ThisXY, Array1600),
    NewMapAcc = MapAcc#{ThisXY => ThisLane},
    NewXY     = {xy, X, Y + 1},
    lane_map(Array1600, NewMapAcc, NewXY).
```

Step 2 (making a new lane map by folding over XY-pairs and querying the
original lane map) also works exactly how you expect:

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L621-L669

-spec new_lane_map(LaneMap, MapAcc, Coord) -> NewLaneMap
    when LaneMap    :: #{XY := Lane},
         MapAcc     :: LaneMap,
         Coord      :: XY,
         NewLaneMap :: LaneMap,
         XY         :: {xy, X :: 0..4, Y :: 0..4},
         Lane       :: <<_:64>>.
%% @private
%% The effect of this step is to rearrange the lanes
%%
%% Result[X, Y] = Input[X + 3*Y, X]
%%
%% (mod 5 of course)
%% @end

% terminal case, end of array
new_lane_map(OrigLaneMap, MapAcc, ThisXY = {xy, 4, 4}) ->
    OrigXY   = xytrans(ThisXY),
    ThisLane = maps:get(OrigXY, OrigLaneMap),
    FinalMap = MapAcc#{ThisXY => ThisLane},
    FinalMap;
% end of Y value, set Y to 0 and increment X
new_lane_map(OrigLaneMap, MapAcc, ThisXY = {xy, X, 4}) ->
    OrigXY    = xytrans(ThisXY),
    ThisLane  = maps:get(OrigXY, OrigLaneMap),
    NewMapAcc = MapAcc#{ThisXY => ThisLane},
    NewXY     = {xy, X + 1, 0},
    new_lane_map(OrigLaneMap, NewMapAcc, NewXY);
% general case: increment Y value
new_lane_map(OrigLaneMap, MapAcc, ThisXY = {xy, X, Y}) ->
    OrigXY    = xytrans(ThisXY),
    ThisLane  = maps:get(OrigXY, OrigLaneMap),
    NewMapAcc = MapAcc#{ThisXY => ThisLane},
    NewXY     = {xy, X, Y + 1},
    new_lane_map(OrigLaneMap, NewMapAcc, NewXY).



-spec xytrans(ResultXY) -> InputXY
    when ResultXY :: XY,
         InputXY  :: XY,
         XY       :: {xy, X :: 0..4, Y :: 0..4}.
%% @private
%% Result[X, Y] = Input[X + 3*Y, X]
%%
%% See NIST doc, pp. 14

xytrans({xy, X, Y}) ->
    {xy, (X + 3*Y) rem 5, X}.
```

Step 3 (converting the lane map back into a 1600-bit flat array) also works
exactly how you expect. `xyset/3` is a coordinate system function I wrote,
which you will learn about in [the coordinate system
section](#inner-keccak-coordinate-system).

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L673-L703

-spec lane_map_to_arr1600(LaneMap, Array1600Acc, Coord) -> Array1600
    when LaneMap      :: #{XY := Lane},
         Array1600Acc :: Array1600,
         Coord        :: XY,
         Array1600    :: <<_:1600>>,
         XY           :: {xy, X :: 0..4, Y :: 0..4},
         Lane         :: <<_:64>>.
%% @private
%% inverse of lane_map/3
%%
%% it would probably faster to concatenate an accumulator, but that requires
%% iterating in the correct order, and i'm more comfortable calling xyset/3
%% @end

% terminal case, end of array
lane_map_to_arr1600(LaneMap, Array1600Acc, ThisXY = {xy, 4, 4}) ->
    ThisLane          = maps:get(ThisXY, LaneMap),
    FinalArray1600Acc = xyset(ThisXY, Array1600Acc, ThisLane),
    FinalArray1600Acc;
% end of Y value, set Y to 0 and increment X
lane_map_to_arr1600(LaneMap, Array1600Acc, ThisXY = {xy, X, 4}) ->
    ThisLane        = maps:get(ThisXY, LaneMap),
    NewArray1600Acc = xyset(ThisXY, Array1600Acc, ThisLane),
    NewXY           = {xy, X + 1, 0},
    lane_map_to_arr1600(LaneMap, NewArray1600Acc, NewXY);
% general case: increment Y value
lane_map_to_arr1600(LaneMap, Array1600Acc, ThisXY = {xy, X, Y}) ->
    ThisLane        = maps:get(ThisXY, LaneMap),
    NewArray1600Acc = xyset(ThisXY, Array1600Acc, ThisLane),
    NewXY           = {xy, X, Y + 1},
    lane_map_to_arr1600(LaneMap, NewArray1600Acc, NewXY).
```


### Inner Keccak: chi stage

The effect of this step is to xor each bit with a non-linear function of two
other nearby bits. Specifically,

```
NewBit = lxor(Bit,
              land(lnot(BitToTheRight),
                   Bit2ToTheRight))
```

where `lnot`, `lxor`, and `land` are the standard binary logical operations.

Beware the [Greek letter pitfall][greek-letter-pitfall].

![[NIST standard][nist-standard], pp. 15](./chi.png)

The code works pretty much the way you expect.

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L714-L771

-spec chi(Array1600) -> NewArray1600
    when Array1600    :: <<_:1600>>,
         NewArray1600 :: <<_:1600>>.
%% @private
%% The chi step. The following transformation is applied to each bit
%%
%% NewBit = lxor(Bit,
%%               land(lnot(BitToTheRight),
%%                    Bit2ToTheRight))

chi(Array1600) ->
    chi(Array1600, Array1600, 0).


-spec chi(Array1600, OldArray1600, Idx0) -> NewArray1600
    when Array1600    :: <<_:1600>>,
         OldArray1600 :: <<_:1600>>,
         Idx0         :: non_neg_integer(),
         NewArray1600 :: <<_:1600>>.
%% @private
%% The chi step. The following transformation is applied to each bit
%%
%% NewBit = lxor(Bit,
%%               land(lnot(BitToTheRight),
%%                    Bit2ToTheRight))
%%
%% FIXME: Could be made more efficient by operating on lanes

chi(Array1600, OrigArray, ThisIdx0) when 0 =< ThisIdx0, ThisIdx0 =< 1599 ->
    ThisXYZ      = {xyz,             ThisX  , ThisY, ThisZ} = idx0_to_xyz(ThisIdx0),
    RightXYZ     = {xyz,       right(ThisX) , ThisY, ThisZ},
    Right2XYZ    = {xyz, right(right(ThisX)), ThisY, ThisZ},
    ThisBit      = xyzth(ThisXYZ  , Array1600),
    RightBit     = xyzth(RightXYZ , OrigArray),
    Right2Bit    = xyzth(Right2XYZ, OrigArray),
    NewBit       = lxor(ThisBit,
                        land(lnot(RightBit),
                             Right2Bit)),
    NewArray1600 = xyzset(ThisXYZ, Array1600, NewBit),
    NewIdx0      = ThisIdx0 + 1,
    chi(NewArray1600, OrigArray, NewIdx0);
% terminal case
chi(Array1600, _, 1600) ->
    Array1600.


lxor(0, 0) -> 0;
lxor(0, 1) -> 1;
lxor(1, 0) -> 1;
lxor(1, 1) -> 0.

land(0, 0) -> 0;
land(0, 1) -> 0;
land(1, 0) -> 0;
land(1, 1) -> 1.

lnot(0) -> 1;
lnot(1) -> 0.
```

### Inner Keccak: iota stage

Remember that there are 24 rounds, and `iota` is the only step that cares about which round we are in.

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L367-L368
rnd(RoundIdx0, Sponge) ->
    iota(RoundIdx0, chi(pi(rho(theta(Sponge))))).
```

The effect of this step is to xor the `{xy, 0, 0}` lane with a constant
bitstring, and there's a different bitstring for each round

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L779-L790

-spec iota(RoundIdx0, Array1600) -> NewArray1600
    when RoundIdx0    :: 0..23,
         Array1600    :: <<_:1600>>,
         NewArray1600 :: <<_:1600>>.
%% @private
%% iota xors the 0,0 lane by a round constant which depends on the round

iota(RoundIdx0, Array1600) ->
    <<Lane00_int:64>>     = xyth({xy, 0, 0}, Array1600),
    ThisRoundConstant_int = round_constant_int(RoundIdx0),
    NewLane00_bytes       = <<(Lane00_int bxor ThisRoundConstant_int):64>>,
    xyset({xy, 0, 0}, Array1600, NewLane00_bytes).
```

Beware the [iota step round constant
pitfall](#pitfall-iota-step-round-constant-table). **DO NOT BLINDLY COPY THIS
ROUND CONSTANT TABLE!  IT MAY HAVE OPPOSITE BIT-ENDIANNESS FROM WHAT WOULD MAKE
SENSE FOR YOUR CODE!**

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L824-L847

%% DO NOT BLINDLY COPY THIS ROUND CONSTANT TABLE!  IT MAY HAVE OPPOSITE
%% BIT-ENDIANNESS FROM WHAT WOULD MAKE SENSE FOR YOUR CODE!

round_constant_int( 0) -> 9223372036854775808; %% 16#8000000000000000 !!
round_constant_int( 1) -> 4684025087442026496;
round_constant_int( 2) -> 5836946592048873473;
round_constant_int( 3) -> 281479271677953;
round_constant_int( 4) -> 15060318628903649280;
round_constant_int( 5) -> 9223372041149743104;
round_constant_int( 6) -> 9295711110164381697;
round_constant_int( 7) -> 10376575016438333441;
round_constant_int( 8) -> 5836665117072162816;
round_constant_int( 9) -> 1224979098644774912;
round_constant_int(10) -> 10376575020733300736;
round_constant_int(11) -> 5764607527329202176;
round_constant_int(12) -> 15060318633198616576;
round_constant_int(13) -> 15060037153926938625;
round_constant_int(14) -> 10448632610476261377;
round_constant_int(15) -> 13835339530258874369;
round_constant_int(16) -> 4611967493404098561;
round_constant_int(17) -> 72057594037927937;
round_constant_int(18) -> 5764888998010945536;
round_constant_int(19) -> 5764607527329202177;
round_constant_int(20) -> 9295711110164381697;
round_constant_int(21) -> 72339069014638593;
round_constant_int(22) -> 9223372041149743104;
round_constant_int(23) -> 1153202983878524929.
```

### Inner Keccak: Coordinate System

![[NIST standard][nist-standard], page 11](./spongecoords.png)

Inner Keccak thinks of the 1600-bit input array as a 5x5x64 3D array.  This
section provides a variety of helper functions to talk about the array using
the X,Y,Z coordinate system.

The coordinate system is toroidal, meaning that each coordinate is "modded
down" to be in the approprate range.  For instance, the X-coordinate "to the
right" of X=4 is X=0. And likewise, the coordinate "behind" Z=63 is Z=0. See
the section on directionality conventions.

#### Inner Keccak Coordinate System: Vocabulary

![[NIST standard][nist-standard], p.8](./spongeparts.png)

##### 3D state:

- The **state** is the entire 5x5x64 array

##### 0D subsets of the state:

- a **bit** is a single bit in the array given by an X,Y,Z coordinate triple
- we frequently need to query individual bits (`xyzth/3`) and update them (`xyzset/3`)
- the first tricky part is the directionality conventions (`left/1`/`right/1`, `up/1`/`down/1`,
  `front/1`/`behind/1`)
- the second tricky part is convention for how the 3D bit array maps back and forth
  between the 1D flat bit array in memory (`idx0_to_xyz/1` and `xyz_to_idx0/1`)

##### 1D subsets of the state:

-   a **row**
    -   is a 5-bit array
    -   given by a Y,Z coordinate pair in range `{0..4, 0..63}`
    -   you should think of a row as being internally indexed with an X
        coordinate ranging in `0..4`
    -   we do not ever operate on rows or need to query them
-   a **column**
    -   is a 5-bit array
    -   given by an X,Z coordinate pair in range `{0..4, 0..63}` (see `xzth/2`)
    -   you should think of a column as being internally indexed with a Y
        coordinate ranging in `0..4`
    -   we need to query columns in the [theta
        step](#inner-keccak-theta-stage), but do not ever need to update them,
        so there is only a query function (`xzth/2`)

-   a **lane**
    -   is a 64-bit array
    -   given by an X,Y coordinate pair in range `{0..4, 0..4}` (see `xyth/2`)
    -   you should think of a lane as being internally indexed with a Z
        coordinate ranging in `0..63`.
    -   [rho](#inner-keccak-rho-stage), [pi](inner-keccak-pi-stage), and
        [iota](#inner-keccak-iota-stage) each operate on lanes
    -   there is both a query function (`xyth/2`) and an update function
        (`xyset/2`)

##### 2D subsets of the state:

There are terms for 2-dimensional subsets of the state, but they are never
queried or updated, so there's no code here that corresponds to them.

#### Inner Keccak Coordinate System: Code

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L919-L1165

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CONVERTING BETWEEN XYZ-INDICES AND 0-INDICES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec idx0_to_xyz(Idx0) -> XYZ
    when Idx0 :: 0..1599,
         XYZ  :: {xyz, X :: 0..4, Y :: 0..4, Z :: 0..63}.
%% @private
%% Convert a 0-index to an XYZ-index
%% @end

idx0_to_xyz(Idx0) ->
    % it's sort of retarded endian notation
    % drunk endian notation
    %   YXZ
    % yes, that order
    % Z is in the range 0..63
    % X is in the range 0..4
    % Y is in the range 0..4
    {Q1, Z} = {Idx0 div 64, Idx0 rem 64},
    {Q2, X} = {  Q1 div  5,   Q1 rem  5},
    { 0, Y} = {  Q2 div  5,   Q2 rem  5},
    {xyz, X, Y, Z}.



-spec xyz_to_idx0(XYZ) -> Idx0
    when XYZ  :: {xyz, X :: 0..4, Y :: 0..4, Z :: 0..63},
         Idx0 :: 0..1599.
%% @private
%% Convert an XYZ-index into a 0-index
%% @end

xyz_to_idx0({xyz, X, Y, Z}) ->
    % reverse of the above
    % drunk endian notation
    %   YXZ
    % to get the "X place", multiply X by 64
    % to get the "Y place", multiply Y by 64*5
    Y*64*5 + X*64 + Z.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DIRECTIONAL TRANSFORMATIONS ON SINGLE COORDINATE VALUES
%%
%% For instance, if you have an X-value and want to get the X-value "to the
%% left", this section contains functions that compute such things.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec left(X) -> XToTheLeft
    when X          :: 0..4,
         XToTheLeft :: 0..4.
%% @private
%% x = left/right
%%        -/+
%% @end

left(0) -> 4;
left(1) -> 0;
left(2) -> 1;
left(3) -> 2;
left(4) -> 3.



-spec right(X) -> XToTheRight
    when X           :: 0..4,
         XToTheRight :: 0..4.
%% @private
%% x = left/right
%%        -/+
%% @end

right(0) -> 1;
right(1) -> 2;
right(2) -> 3;
right(3) -> 4;
right(4) -> 0.



-spec down(Y) -> YBelow
    when Y      :: 0..4,
         YBelow :: 0..4.
%% @private
%% y = down/up
%%        -/+
%% @end

down(0) -> 4;
down(1) -> 0;
down(2) -> 1;
down(3) -> 2;
down(4) -> 3.



-spec up(Y) -> YAbove
    when Y      :: 0..4,
         YAbove :: 0..4.
%% @private
%% y = down/up
%%        -/+
%% @end

up(0) -> 1;
up(1) -> 2;
up(2) -> 3;
up(3) -> 4;
up(4) -> 0.



-spec front(Z) -> ZInFront
    when Z        :: 0..63,
         ZInFront :: 0..63.
%% @private
%% z = front/behind
%%        -/+
%% @end

front(0)                      -> 63;
front(N) when 1 =< N, N =< 63 -> N - 1.



-spec behind(Z) -> ZBehind
    when Z       :: 0..63,
         ZBehind :: 0..63.
%% @private
%% z = front/behind
%%        -/+
%% @end

behind(N) when 0 =< N, N =< 62 -> N + 1;
behind(63)                     -> 0.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 0D BIT ACCESSORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec xyzth(XYZ, Array1600) -> Bit
    when XYZ       :: {xyz, X, Y, Z},
         Array1600 :: <<_:1600>>,
         Bit       :: 0 | 1,
         X         :: 0..4,
         Y         :: 0..4,
         Z         :: 0..63.
%% @private
%% Fetch the bit at the given X, Y, Z coordinate triple
%% @end

xyzth(XYZ, Array1600) ->
    Idx0 = xyz_to_idx0(XYZ),
    <<_Skip:Idx0, Bit:1, _Rest/bitstring>> = Array1600,
    Bit.



-spec xyzset(XYZ, Array1600, NewBit) -> NewArray1600
    when XYZ          :: {xyz, X, Y, Z},
         Array1600    :: <<_:1600>>,
         NewBit       :: 0 | 1,
         NewArray1600 :: Array1600,
         X            :: 0..4,
         Y            :: 0..4,
         Z            :: 0..63.
%% @private
%% Replace the bit at {X, Y, Z} with the new bit
%% @end

xyzset(XYZ, Array1600, NewBit) ->
    Idx0 = xyz_to_idx0(XYZ),
    <<Pre:Idx0, _Bit:1, Post/bitstring>> = Array1600,
    <<Pre:Idx0, NewBit:1, Post/bitstring>>.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1D SUBSET ACCESSORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec xzth(XZ, Bits) -> Column
    when XZ     :: {xz, X, Z},
         X      :: 0..4,
         Z      :: 0..63,
         Bits   :: <<_:1600>>,
         Column :: <<_:5>>.
%% @private
%% Fetch the column at the given X, Z coordinate pair
%% @end

xzth({xz, X, Z}, Bits) ->
    % just grab them one at a time
    << <<( xyzth({xyz, X, Y, Z}, Bits) ):1>>
    || Y <- lists:seq(0, 4)
    >>.



-spec xyth(XY, Array1600) -> Lane
    when XY        :: {xy, X, Y},
         Array1600 :: <<_:1600>>,
         Lane      :: <<_:64>>,
         X         :: 0..4,
         Y         :: 0..4.
%% @private
%% Grab the lane at the given X, Y coordinate pair.
%% @end

xyth({xy, X, Y}, Array1600) ->
    << <<( xyzth({xyz, X, Y, Z}, Array1600) ):1>>
    || Z <- lists:seq(0, 63)
    >>.



-spec xyset(LaneXY, Array1600, NewLane) -> NewArray1600
    when Array1600    :: <<_:1600>>,
         LaneXY       :: {xy, 0..4, 0..4},
         NewLane      :: <<_:64>>,
         NewArray1600 :: <<_:1600>>.
%% @private
%% Take the original array, and swap out the lane at the given x,y coordinate
%% with the new given lane.
%%
%% The lane will be represented continuously so we can do a hack
%% @end

% special case when it's the last lane
% grab the final 64 bits off the original array and replace them with the new lane
xyset(_LaneXY = {xy, 4, 4}, <<Pre:(1600 - 64), _:64>>, NewLane) ->
    <<Pre:(1600 - 64), NewLane/bitstring>>;
% general case, grab the shit before the lane, grab the shit after the lane
% replace the shit in the middle
xyset(_LaneXY = {xy, LaneX, LaneY}, OriginalArray, NewLane) ->
    FirstBitOfLane_Idx0    = xyz_to_idx0({xyz, LaneX, LaneY, 0}),
    FirstBitAfterLane_Idx0 = xyz_to_idx0({xyz, LaneX, LaneY, 63}) + 1,
    NumberOfBitsBeforeTheLane    = FirstBitOfLane_Idx0,
    NumberOfBitsIncludingTheLane = FirstBitAfterLane_Idx0,
    <<PreLane:NumberOfBitsBeforeTheLane   ,         _/bitstring>> = OriginalArray,
    <<      _:NumberOfBitsIncludingTheLane, AfterLane/bitstring>> = OriginalArray,
    Result = <<PreLane:NumberOfBitsBeforeTheLane, NewLane/bitstring, AfterLane/bitstring>>,
    Result.
```



## Conclusion

What do you want, a cookie?

[coord-system]: #inner-keccak-coordinate-system
[greek-letter-pitfall]: #pitfall-greek-letter-steps-require-two-copies-of-the-sponge-to-compute
[german-lecture]: https://www.youtube.com/watch?v=JWskjzgiIa4
[german-lecture-notes]: https://www.crypto-textbook.com/download/Understanding-Cryptography-Keccak.pdf
[nist-standard]: https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf
[rc-erl]: https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/rc.erl
[^alg5]: [NIST standard][nist-standard], Algorithm 5, pp 16.
[^tbl]: From [the German guy's lecture notes][german-lecture-notes], pp. 12.
[^sponge]: Source for photo: https://www.flickr.com/photos/30478819@N08/46410395345
