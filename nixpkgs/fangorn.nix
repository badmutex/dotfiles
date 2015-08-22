{

  packageOverrides = pkgs: with pkgs;
    let

       mylib = callPackage ./_lib.nix { } ;

       myDevEnvs = callPackage ./devenv.nix { } ;
       myXmonad = callPackage ./xmonad.nix { } ;
       myGames = callPackage ./games.nix { } ;
       myCloudFS = callPackage ./cloudfs.nix { } ;
       myTools = callPackage ./tools.nix {
         withLibreOffice=false;
       };
       myX11 = callPackage ./x11.nix { } ;
       myWeb = callPackage ./web.nix { } ;

       all =
         myDevEnvs ++ myXmonad ++ myGames
         ++ myCloudFS ++ myTools ++ myX11 ++ myWeb
         ;

       office = [
         kde4.gwenview
        ];

    in {
      badi = mylib.mkEnv all;
    };
}
//
import ./settings.nix
