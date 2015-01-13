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
