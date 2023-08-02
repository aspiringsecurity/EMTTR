# sidekick examples

# TODO

- form a spend transaction
- transfer money on testnet
- retrieve transaction info

# Build Prereqs

Assuming Ubuntu 18.04. Adapt these instructions for your own system.

You need

- `npm` to build TypeScript
- `tsc` to compile
- [`jx`][jx] to orchestrate the build properly
- [sidekick][sk] as a dependency
- Python 3.6 or later to run `jx`

[jx]: https://gitlab.com/pharpend/jx/-/tree/master/#jx-secure-typescript-package-manager

Steps

    # install node and tsc
    sudo snap refresh
    sudo snap install node --channel 18/stable
    npm install -g typescript
    # install dependencies
    mkdir vanillae && cd vanillae
    git clone https://gitlab.com/pharpend/jx
    git clone https://gitlab.com/pharpend/sidekick
    chmod u+x ~/.local/bin/jx
    git clone https



## Protip: avoid using `sudo npm install -g`

If you want to avoid using sudo

```
mkdir ~/.npm-packages
npm config set prefix "${HOME}/.npm-packages"
```

Edit `~/.bashrc` or `~/.zshrc` with

```
NPM_PACKAGES="${HOME}/.npm-packages"
export PATH=$NPM_PACKAGES/bin:$PATH
```

## How to build

You need 

- [TypeScript compiler `tsc`](https://www.typescriptlang.org/download)
- [HTTP logging server vlogd](https://gitlab.com/pharpend/vlogd)
- [Build tool `jx`](https://gitlab.com/pharpend/jx)
- Build [sidekick](https://gitlab.com/pharpend/sidekick) separately
- Python 3.6 or later
- `make`

#### Build

You need to build [sidekick](https://gitlab.com/pharpend/sidekick) separately

```
make
```

#### View

Many of the examples assume there is an HTTP server at `localhost:8841`
which responds positively to POST requests at `/`

[vlogd](https://gitlab.com/pharpend/vlogd) is such a server, but you can roll
your own if you want.

```
make serve
firefox localhost:8001
```



# Build

    make
