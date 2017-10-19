{ pkgs, stdenv, ... }:

with pkgs.haskell.packages.ghc802;

ghcWithHoogle (
  hspkgs: with hspkgs; [
    cabal-install
    stack
    alex
    happy
    hscolour
    hasktags
    # stylish-haskell
    structured-haskell-mode
    ghc-mod ## not usable right now
    hlint
    hdevtools
  ]
)
