{ pkgs }:

with pkgs;

let
  myGitTools = callPackage ./gitTools.nix { };
  python27DevEnv = callPackage ./devenv/python27.nix { };

in

myGitTools ++
[
  emacs
  python27DevEnv
]
