{

  packageOverrides = pkgs:
    let

      myGitTools = with pkgs.gitAndTools; [
        git
        hub
        git-extras
        git-remote-hg
        git2cl
        gitFastExport
        gitflow
        qgit
        topGit
      ];

      all = [
        pkgs.colordiff
        pkgs.nix-repl
      ]
      ++ myGitTools
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
