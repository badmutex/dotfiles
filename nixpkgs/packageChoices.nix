{ pkgs, config
, ... }:

let

  inherit (builtins) elem;
  inherit (pkgs.stdenv) isLinux;

  isNixOS = builtins.pathExists /etc/NIXOS;
  hostname = builtins.getEnv "HOSTNAME";

  isHomeMachine = elem hostname ["fangorn"];

in
{
  withChrome      = isLinux;
  withChromium    = isLinux;
  withDropbox     = isNixOS;
  withEvince      = isLinux;
  withGames       = isHomeMachine;
  withInkscape    = isLinux;
  withLatex       = isLinux;
  withLibreOffice = false;
  withMega        = false;
  withPopfile     = isHomeMachine;
  withSpotify     = false;
  withSynergy     = ! isNixOS;
  withWesnoth     = isHomeMachine;
  withX11         = isNixOS;
  withXmonad      = isNixOS;
  withYubikey     = isLinux;
}
