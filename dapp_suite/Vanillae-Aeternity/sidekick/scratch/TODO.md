- DONE start skylight in ignore state
- DONE add `listen` and `ignore` calls
- DONE fix examples
- polish examples with prose so it's more clear what is going on
- fix documentation
- test against other browsers/operating systems
- work out crash-handling examples
- make a documentation release
- (maybe) add `console.debug` messages
- add internal logging
- add POST logging
- add safe error handling
- future: add JR logging

- DONE rpc errors enumerate
- flush out logging and safety
- timeout errors
- write out everything
- logging
- we might want something like traverse... hmm... think about it
- look at "address.get" instead of "address.subscribe"

---

- async monad structure



alpha:


- DONE add more sophisticated examples to README
    - sign a spendtx
    - mention that anything else you're going to want the user to do
      (e.g. sign a smart contract) is just telling the user to sign a
      transaction
    - you are responsible for forming the calldata and the
      transaction
- DONE `site_tree` target for sidekick
- DONE check all the FIXMEs and TODOs and NOTEs
- DONE polish examples
- DONE add constants for default timeouts

beta:

-   work out idioms for application developers to handle errors
-   add examples showing how the user should handle/differentiate
    between different types of errors that can occur (e.g. timeouts,
    user-rejections).

    possibly need to write classes or something for each different type of
    error. not sure.

-   one more pass over documentation
-   double check when confirm-connect modal is given to user
-   test against other browsers and other operating systems

-   DONE expose low-level skylight primitives in sidekick module (`connect`,
    `detect`, `address`, `listen`, `ignore`).
-   DONE get rid of all possible instances of `as` (some are necessary
    because TypeScript is dumb, a handful are because I am dumb. The
    because-I-am-dumb ones should be fixed)

---

DONE:

- DONE propagate timeout shit into examples

- DONE awcp errors

- DONE timing (done except need to double check when confirm-connect
  modal is given to user)

I am *probably* not going to fix the message queue threading thing.
Because any fix requires completely gutting the push/pop idiom which
is so nice in the MsgQ and simplifies the code dramatically. I can't
imagine an instance that would actually occur in practice where that
would be an issue. And people who are going to do shit like that
deserve to have to write their own secondary state layer. So \shrug.

---

- every single function that interacts with the wallet needs a
  timeout parameter. it cannot simply be the same for every single
  thing. the user might take 5 minutes to review a transaction, but
  putting a 5 minute timeout on "detect if the wallet is there" is
  really fucking stupid and fucking retarded as shit.

- awcp needs to be modified to have error types as the returns for
  everything
- somewhere in the callchain, either in skylight, msgr, or msgq,
  errors need to be caught and exceptions need to be thrown
- throwing an exception is good enough, we don't need to get fancier
- the lack of real sum types and "crash fast" idioms in typescript is
  a real bummer. can't blame typescript. there's no real way to fix
  this, this is simply a deficiency of the JavaScript idiom.

  this could be fixed in a erlang-on-wasm language, but maintaining
  that would be way more of a pain than working around this.

  so, alas

- add more examples to readme
- all examples need to use only top level functions
- test in other browsers
- make errors great again

---

- DONE (it doesn't, it needs the network id even just to do a
  signature; why is beyond me, but i will probably figure out when we
  do jaeck russell [will find out if it's an aeternity constraint, or
  if this is a superhero constraint; i.e. is superhero imposing this
  constraint for no reason, or is there something at the protocol
  level where in order to sign a transaction, it needs the network
  id? don't know, but will find out I suppose]).

  test to see if noprop transaction works if we remove the network id
  (it should right? it's just cryptography. who knows? satan. because
  satan wrote this code)


- DONE constants like network id need to be passed in as top level
  parameters to inputs
- DONE explain awcp

- DONE get examples to work

---

Sidekick should

-   know how to:

    - build a distribution
    - build its documentaiion

- "examples" should be converted to a test suite

    Look into:

    - jest
    - jasmine
    - chai
    - mocha

    update: these test suites are retarded

- move examples back in here but frame them as tests

Sidekick should NOT

- version control documentation
- version control distribution

- vanillae website should contain documentation
- should host examples (maybe?) (maybe make a separate distribution
  of just examples?)

- I think maybe the constraint of "download it, run a makefile and it
  just works" with no non-standard tooling is too severe.


---

- DONE remove webpack jizz
- DONE separate things that talk to the network in `parasite` package
- fix examples
- add js doc comments
- generally clean up code
- add "info pages" type things
- make website with documentation
- figure out the right way to serve both js-dist and typescript
  source tree so that source maps work as expected


---

- separate types for RPC requests and RPC responses (in particular
  the `result` and `params` properties)

  See <https://www.jsonrpc.org/specification>

- change vim theme
- msgq really is specialized to JSON RPC 2.0

- figure out how to turn off "implicit null" warnings in just the
  `msgq.ts` files

- fill out stuff from the schema: <https://github.com/aeternity/aepp-sdk-js/blob/develop/src/utils/aepp-wallet-communication/schema.ts>

- get a contract page working

- fix the obnoxious styling

- factor out the stylesheet

- figure out a better build/directory structure

- in particular, we're at the point where sidekick needs to be
  separated from the examples "packed"

  - ok, I know how to do that
  - have sidekick compile into the `sidekick-js-dist` directory or
    something
  - move javascript examples into inline scripts?
  - hmm
  - I don't like either approach
  - i need to pee
  - and brush my teeth
  - and have a think
  - and get more coffee
  - but I really need to pee
  - bye

- make the "do a transfer example" complete (show the transfer, have
  a "do another transfer" thing)

- clearly define the protocol, and how the different components of
  the protocol compose

  - the Window messaging infrastructure (Link: mozilla docs)
  - the JSON RPC 2.0 (link: https://www.jsonrpc.org/specification)
  - the domain-specific messaging protocol (link: random schema file
    on aeternity/aepp-sdk-js GitHub)

- `msg_protocol/gen_rpc2.ts`:
    - `gen_rpc2.Request` and `gen_rpc2.Response` (generic types)
- `msg_protocol/window_messages.ts`
    - this is simply the `to_aepp` and `to_waellet` distinctions

- TypeScript generics:
  https://www.typescriptlang.org/docs/handbook/2/generics.html

- `awcp`: aepp-waellet communication protocol
- `awcp_aepp`: aepp end of AWCP

- I think a msgr module that does "send a message with this type and
  get a response back with another type" would be appropriate

  going to commit


- implement `msgr` according to spec
- refactor skylight to use msgr
- do something similar for node api
- make sidekick the thing that exports functions that interface
  between Skylight, AeNode, and the Compiler

