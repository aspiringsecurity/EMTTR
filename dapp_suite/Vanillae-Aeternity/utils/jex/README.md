# Jex: simple TypeScript/JavaScript packaging system

Jex is a simple packaging/dependency system for TypeScript/JavaScript
projects.

Jex is very much a work in progress, so these instructions are subject to
change.

As of now, Jex is a glorified shell script that automates a lot of the tedium
in building sidekick, JR, etc.

The long-term goal is to completely remove any dependency on NPM.  NPM comes
with a lot of unfixable security issues that present an unacceptable risk in a
business context, which is the focus of the Vanillae project.

## Installation

1.  `git clone https://github.com/aeternity/Vanillae.git`
2.  [Install Erlang and zx](https://www.bitchute.com/video/1gCvcoPUR7eJ/)
3.  Edit `~/.bashrc` (or `~/.zshrc` or whatever) and add

    ```
    alias jex="zx rundir ~/path/to/Vanillae/utils/jex"
    ```

## Usage

```
Jex: simple JavaScript packaging system

COMMANDS:
  man             show the manual
  dwim-           init, pull, build
  dwim+           init, pull, build, mindist, push
  cfgbarf         barf out the jex.eterms file (mostly to make sure it parses correctly)
  echo home       echo $HOME
  echo jexdir     echo $HOME/.jex
  echo devdir     echo $HOME/.jex/dev
  echo pkgname    name of current package
  echo pkgdir     echo $HOME/.jex/dev/realm-name-X.Y.Z
  echo deps       list dependencies of current package
  echo pathof PKG list the path to PKG or 
  init            mkdir -p $HOME/.jex/dev
  build           tsc && cp -r ./src/jex_include ./dist/
      -w, --weak      continue building even if tsc fails
      -f, --force     use cp -rf instead of cp -r
  mindist         mkdir jex_mindist && cp -r src jex_mindist && cp -r dist jex_mindist && rm -r jex_mindist/src/jex_include
      -f, --force     use cp -rf instead of cp -r
  push            rsync -a jex_mindist/ PKGDIR
  ls              ls $HOME/.jex/dev
  tree            tree $HOME/.jex/
  rmpkg PKG       rm -r $HOME/.jex/dev/PKG
  pull            pull each dependency into src/jx_include
```
