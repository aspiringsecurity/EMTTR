# How to install NPM on Ubuntu 18.04

The default `nodejs` package on Ubuntu installs an ancient version of Node/NPM
that often doesn't work properly with contemporary code.

As of the time of writing (October 12, 2022), Ubuntu's repository contains Node
8.10.0, which is 6 years out of date.

To install the latest version of Node (currently: 18.10.0)

```
sudo snap refresh
sudo snap install node --channel 18/stable
```

You can use `snap info node` to get more information about the latest available
versions.

You can use `node --version` to verify you are running the most recent version
of Node.

```
$ node --version
v18.10.0
```


# How to install packages from NPM on unix without using `sudo`

To install an executable package from NPM (such as the TypeScript compiler),
you use `npm install -g PACKAGE`. By default, this attempts to install
`PACKAGE` in a privileged directory, thus requiring `sudo` to execute
successfully.

In order to avoid running NPM as root, do the following:

```
mkdir ~/.npm-packages
npm config set prefix "${HOME}/.npm-packages"
```

Edit `~/.bashrc` or `~/.zshrc` with

```
NPM_PACKAGES="${HOME}/.npm-packages"
export PATH=$NPM_PACKAGES/bin:$PATH
```

Now, `npm install -g` will install to `~/.npm-packages`

```
npm install -g typescript
```
