{ pkgs
, ... }:

with pkgs;

let
  myGitTools = callPackage ./gitTools.nix { };
  python27DevEnv = callPackage ./devenv/python27.nix { };
  haskellDevEnv = callPackage ./devenv/haskell.nix { };
in

myGitTools ++
haskellDevEnv ++
[
  emacs
  gcc
  silver-searcher
  colordiff
  python27DevEnv
  zlib
]
