{ pkgs, config
, ... }:

let

  inherit (builtins) elem;
  inherit (pkgs.stdenv) isLinux;

  envHostname = builtins.getEnv "HOSTNAME";
  nixosConfig  = pkgs.callPackage /etc/nixos/configuration.nix { };

  # a bit of a hack
  isNixOS = builtins.pathExists /etc/NIXOS;

  hostname =
    if isNixOS
    then nixosConfig.networking.hostName
    else envHostname;

  isHomeMachine = elem hostname ["fangorn"];

in
{
  withDropbox     = isHomeMachine;
  withGames       = isHomeMachine;
  withLatex       = isLinux;
  withLibreOffice = false;
  withSpotify     = isLinux;
  withSynergy     = ! isNixOS;
  withX11         = isNixOS;
  withXmonad      = isNixOS;
}
