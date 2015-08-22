{ pkgs, stdenv
, withDropbox ? stdenv.isLinux
}:

with stdenv.lib;
with pkgs;
optional withDropbox dropbox
