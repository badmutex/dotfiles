{ pkgs, stdenv
, withDropbox ? stdenv.isLinux
, withMega ? stdenv.isLinux
, ...}:

with pkgs;

let
  inherit (stdenv.lib) optional;
in
optional withDropbox dropbox
++ optional withMega megatools
