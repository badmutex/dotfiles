{ pkgs, stdenv, ... }:

with pkgs.haskell.packages;

let
  hs = ghc7102.ghcWithPackages (
    hspkgs: with hspkgs; [ cabal-install stack alex happy ]
  );

in

[ hs ]
