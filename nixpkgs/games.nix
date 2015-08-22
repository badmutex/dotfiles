{ pkgs }:

let

  wesnoth = pkgs.callPackage ./apps/wesnoth.nix { };

in
[
  wesnoth
]
