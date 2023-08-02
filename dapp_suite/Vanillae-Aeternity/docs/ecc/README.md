# Elliptic Curve Cryptography

by Peter Harpending

In the context of cryptography, all numbers are integers (math integers that
is, not machine integers) unless stated otherwise.

## How cryptography works in general

All cryptography schemes are based on some computational asymmetry. There is
always some function, let's call it `f`, which has good "one-way" properties:

- `f` is "injective": if `Y = f(X)`, then there is no input `X2` distinct from
  `X` that also maps down to `Y`

  (sometimes these functions are not strictly injective, but instead have the
  property that the likelihood of a collision is extremely low; example:
  "hashing" functions like SHA or Keccak)

- for a given input `X`, computing `Y = f(X)` is very easy
- suppose instead that you know `Y` and `f` but not `X`; that is, you know that
  `Y = f(X)` for *some* `X` but you don't know what that `X` is. A good one-way
  function has the property that computing `X` is theoretically possible, but
  is either prohibitively difficult, *or* is prohibitively difficult *absent
  some secret piece of information* (usually this is called a "private key")

The way that cryptography works is that `X` is kept private, and `Y` is what is
communicated in public.  You need some special piece of information (the
"private key") to reverse the function and figure out what `X` was.

In a sense, all there is to cryptography is the concept of "hashing" (beware,
using this term loosely in a more general way than others do).  These hashes
vary only in the details of their structure.  These details give rise to
certain properties which may or may not be desireable depending on the usage
case.

Some "hashes" are deliberately unstructured so that inverting the hash
(figuring out what the input was, given the output) is effectively impossible.
(Typically when you hear about "hashing", it means only this specific type of
unstructured "hashing").  A case where this property is useful would be storing
passwords in a database: you want to be able to check whether or not a given
password attempt is correct, but you don't want to expose your users' passwords
in the event of a data breach.

### Example: Diffie Hellman

An example of where structure might be useful is the **Diffie-Hellmann**
system:

- we pick a really big number `M`, called the **modulus**

  (typically you want this modulus to be prime, I will explain why below)

- we pick a number `G` less than `M`, called the **generator**

  this number `G` has the property that the sequence `{G, G*G mod M, G*G*G mod
  M, G*G*G*G mod M, ...}` eventually cycles through all the numbers between `1`
  and `M`

  (Prime moduli `M` are useful because **every** number `G < M` is a generator
  with respect to a prime modulus; will explain later, it's not complicated,
  but not obvious either).

For example, let's pick `M = 13` and `G = 2`

```erl
1> Pow = fun F(X, 0, Mod) -> 1; F(X, N, Mod) -> X * F(X, N-1, Mod) rem Mod end.
#Fun<erl_eval.17.3316493>
7> [Pow(2, N, 13) || N <- lists:seq(1, 13)].
[2,4,8,3,6,12,11,9,5,10,7,1,2]
8> lists:sort([Pow(2, N, 13) || N <- lists:seq(1, 13)]).
[1,2,2,3,4,5,6,7,8,9,10,11,12]
```

So we go

```
2         =    2             =  2  (mod 13)
2*2       =    4             =  4  (mod 13)
2*2*2     =    8             =  8  (mod 13)
2*2*2*2   =   16 =   13 +  3 =  3  (mod 13)
2*2*2*2*2 =   32 =   26 +  6 =  6  (mod 13)
2^6       =   64 =   52 + 12 = 12  (mod 13)
2^7       =  128 =  117 + 11 = 11  (mod 13)
2^8       =  256 =  247 +  9 =  9  (mod 13)
2^9       =  512 =  507 +  5 =  5  (mod 13)
2^10      = 1024 = 1014 + 10 = 10  (mod 13)
2^11      = 2048 = 2041 +  7 =  7  (mod 13)
2^12      = 4096 = 4095 +  1 =  1  (mod 13)
2^13      = 8192 = 8190 +  2 =  2  (mod 13)
```

If we continued, we would just cycle through the same list over and over (try!).

The point is if we know `E`, it's very easy to compute `2^E mod 13`.

What is **not** easy is figuring out what `E` is if we know that `2^E = 5 mod
13`. This is the **discrete log problem**.  It's easy in this case because I
picked small numbers, so you can just check every possibility by hand.  It is
(believed to be) prohibitively difficult to compute `E` if the modulus is large
enough and the generator is not stupid.

So in this "cryptography scheme", your private key would be your (random)
choice of `E`, and your public key would be the number `2^E mod 13`.

The interesting thing is that if I have my own private key `F`, and I publish
`2^F` as my public key, both of us can compute `2^(F*E)`, without knowing each
other's private keys.

- I take your public key `2^E` and raise it to the power `F`
- You take my public key `2^F` and raise it to the power `E`

And crucially, *nobody else can compute this secret key*.

My friend summarized this as "commutative hashes allow the establishment of
shared secrets."  If you and I have a shared secret, then we also have any
number of ways of encrypting messages between the two of us where only we can
decode them.


## What are elliptic curves?

An **\[elliptic curve\]** is defined by an equation

```
y^2 = x^3 + a*x + b   (mod n)
```

A point `(X: integer, Y: integer)` is on the curve if it satisfies the
equation; i.e. `Y*Y - X*X*X - a*X - b` is a multiple of `n`.  The definition of
the curve is the triple `(a, b, n)`.

If we plot an elliptic curve over the real numbers, it looks like this

[diagram]

The operation that we care about on elliptic curves is the "elliptic curve
\[group\] operation", which we will call `ec_grop(Pt1: ec_point, Pt2: ec_point) -> ec_point`.

What matters is that we can `ec_grop` any two points on the curve (including
the same point with itself) and produce a new point on the curve.

[diagram]

If the curve is chosen correctly, there will be (at least one) special point
on the curve which is called a **\[generator\]**. This generator point (let's
call it `G`) has the property that if we `ec_grop` it with itself repeatedly
(`ec_grop(G, ec_grop(G, ec_grop(G, ...)))`), the resulting **\[orbit\]** cycles
through every point on the curve.

### The EC group operation

### Projective Geometry
