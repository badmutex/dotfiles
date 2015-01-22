{

  allowUnfree = true;

  packageOverrides = pkgs: with pkgs;
    let allPkgs = [

      # web
      chromium

      # office
      evince
      kde4.gwenview
      dropbox
      libreoffice
      inkscape

      # latex
      texLiveFull
      biber
      texmaker

      # misc tools
      tmux
      keepass
      colordiff
      unzip
      spotify

      # xmonad
      dmenu
      trayer
      haskellPackages.xmobar
      xcompmgr
      networkmanagerapplet

      # nix-related tools
      nix-repl
      strategoPackages.strategoxt # provides `pp-aterm` for printing .drv files

     ];

     mkEnv = name: paths: buildEnv {
       name = "badi-" + name + "-packages";
       paths = paths;
     };

    in {
      badi = {
        dain = mkEnv "dain" allPkgs;
        fangorn = mkEnv "fangorn" allPkgs;
      };
    };
}