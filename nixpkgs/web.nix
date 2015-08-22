{ pkgs, stdenv
, withChromium ? stdenv.isLinux
}:

with stdenv.lib;
with pkgs;
optional withChromium chromiumDev
