{ pkgs, stdenv, ... }:

with pkgs.haskell.packages;

let
  hs = ghc7102.ghcWithPackages (
    hspkgs: with hspkgs; [
      cabal-install
      stack
      alex
      happy
      hscolour
      hasktags
      stylish-haskell
      structured-haskell-mode
      ghc-mod ## not usable right now
      hlint
      hoogle
      hdevtools
    ]
  );

in

[ hs ]
