{ pkgs, config
, ... }:

let

  inherit (builtins) elem;

  envHostname = builtins.getEnv "HOSTNAME";
  nixosConfig  = pkgs.callPackage /etc/nixos/configuration.nix { };

  # a bit of a hack
  isNixOS = envHostname == "";

  hostname =
    if isNixOS
    then nixosConfig.networking.hostName
    else envHostname;

  isHomeMachine = elem hostname ["fangorn"];

in
{
  withDropbox     = isHomeMachine;
  withGames       = isHomeMachine;
  withLatex       = false;
  withLibreOffice = false;
  withSynergy     = ! isNixOS;
  withX11         = isNixOS;
  withXmonad      = isNixOS;
}
