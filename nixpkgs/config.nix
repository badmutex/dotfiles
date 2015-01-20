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

      # nix-related tools
      nix-repl
      strategoPackages.strategoxt # provides `pp-aterm` for printing .drv files

     ];

    in {
      badi = {
        dain = buildEnv {
          name = "badi-dain-packages";
	  paths = allPkgs;
        };
      };
    };
}
