{


  allowBroken = true;

  allowUnfree = true;

  chromium = {
    enablePepperFlash = true;
    enablePepperPDF = true;
    enableWideVine = true;
  };

  packageOverrides = pkgs:
    let

      myGitTools = pkgs.callPackage ./gitTools.nix { };
      myDevEnvs = pkgs.callPackage ./devenv.nix { };

      all = [
        pkgs.colordiff
        pkgs.nix-repl
        pkgs.chromium
        pkgs.spotify
      ]
      ++ myGitTools
      ++ myDevEnvs
      ;

      mkEnv = name: paths: pkgs.buildEnv {
        name = "badi-" + name + "-packages";
        paths = paths;
      };

    in {
      badi = {
        gambit = mkEnv "gambit" all;
      };
    };
}
