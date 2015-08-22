{ pkgs, stdenv
, withWesnoth ? stdenv.isLinux
 }:

let

  wesnoth = pkgs.callPackage ./apps/wesnoth.nix { };

in
with stdenv.lib;

optional withWesnoth wesnoth
