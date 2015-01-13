{
  packageOverrides = pkgs: with pkgs;
    let allPkgs = [

      # web
      chromium
      firefox

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
