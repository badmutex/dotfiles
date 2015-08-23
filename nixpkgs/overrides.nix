{ pkgs
, stdenv
, ...
}:

with pkgs;

let

  inherit (stdenv.lib) optional;
  mylib = callPackage ./_lib.nix { } ;
  myOptions = callPackage ./_withOptions.nix { };

  args = {inherit pkgs config stdenv;} // myOptions;

in

with myOptions;

let

  myDevEnvs = callPackage ./devenv.nix args ;
  myXmonad = callPackage ./xmonad.nix args ;
  myGames = callPackage ./games.nix args ;
  myCloudFS = callPackage ./cloudfs.nix args ;
  myTools = callPackage ./tools.nix args ;
  myX11 = callPackage ./x11.nix args ;
  myWeb = callPackage ./web.nix args ;

  all =
    myDevEnvs
    ++ myCloudFS
    ++ myTools
    ++ myWeb
    ++ optional withGames myGames
    ++ optional withXmonad myXmonad
    ++ optional withX11 myX11
    ;

  office = [
    kde4.gwenview
  ];

in {
  badi = mylib.mkEnv all;
}
