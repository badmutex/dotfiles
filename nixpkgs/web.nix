{ pkgs, stdenv
, withChromium ? stdenv.isLinux
, ...}:

with pkgs;
let
  inherit (stdenv.lib) optional;
in
optional withChromium chromium
