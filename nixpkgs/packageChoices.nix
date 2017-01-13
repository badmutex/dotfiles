{ pkgs, config
, ... }:

let

  inherit (builtins) elem;
  inherit (pkgs.stdenv) isLinux;

  isNixOS = builtins.pathExists /etc/NIXOS;
  hostname = builtins.getEnv "HOSTNAME";

  isHomeMachine = elem hostname ["fangorn"];
  isWorkMachine = elem hostname ["gambit" "irmo"];

in
{
  withAndroidDev  = isHomeMachine;
  withBox         = hostname == "gambit";
  withChrome      = isLinux;
  withChromium    = false;
  withDigikam     = true;
  withDropbox     = false;
  withEvince      = isLinux;
  withGames       = isHomeMachine;
  withInkscape    = isLinux;
  withLatex       = isLinux;
  withLibreOffice = isLinux;
  withMega        = false;
  withPopfile     = isHomeMachine;
  withPyCharm     = true;
  withSpotify     = true;
  withSynergy     = ! isNixOS;
  withWesnoth     = isHomeMachine;
  withX11         = isNixOS;
  withXmonad      = isNixOS;
  withYubikey     = isLinux;
}
