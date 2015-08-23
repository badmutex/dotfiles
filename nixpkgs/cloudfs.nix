{ pkgs, stdenv
, withDropbox ? stdenv.isLinux
, ...}:

with pkgs;

let
  inherit (stdenv.lib) optional;
in
optional withDropbox dropbox
