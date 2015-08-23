{ pkgs, stdenv
, withWesnoth ? stdenv.isLinux
, ... }:

let

  inherit (stdenv.lib) optional;

  wesnoth = pkgs.callPackage ./apps/wesnoth.nix { };

in

optional withWesnoth wesnoth
