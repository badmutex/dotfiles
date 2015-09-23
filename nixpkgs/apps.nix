{ pkgs, stdenv
, withSpotify ? stdenv.isLinux
, ...}:

let
  inherit (stdenv.lib) optional;
in
with pkgs;

optional withSpotify spotify
