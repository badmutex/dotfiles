{


  allowBroken = false;

  allowUnfree = true;

  packageOverrides = pkgs:
    let

      myGitTools = pkgs.callPackage ./gitTools.nix { };
      myDevEnvs = pkgs.callPackage ./devenv.nix { };

      all = [
        pkgs.emacs
        pkgs.colordiff
        pkgs.nix-repl
	pkgs.keychain
      ]
      ++ myGitTools
      ;

      mkEnv = name: paths: pkgs.buildEnv {
        name = "badi-" + name + "-packages";
        paths = paths;
      };

    in {
      badi = {
        lorien = mkEnv "lorien" all;
      };
    };
}
