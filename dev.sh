#! /usr/bin/env sh

nix-shell shell.nix --run "ghcid -c 'cabal v2-repl exe:keychain'"
