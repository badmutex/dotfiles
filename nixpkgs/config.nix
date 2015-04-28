{

  allowUnfree = true;

  packageOverrides = pkgs: with pkgs;
    let

      pythonDevel = with python27Packages; [
        python
        jedi
        pyflakes
        pip
        virtualenv
      ];

    allPkgs = [

      # web
      chromium

      # office
      emacs
      evince
      kde4.gwenview
      dropbox
      libreoffice
      inkscape
      (callPackage ./apps/mendeley.nix {})
      aspell
      aspellDicts.en
      thunderbird

      # science
      (callPackage ./apps/vmd.nix {})

      # latex
      texLiveFull
      biber
      texmaker

      # misc tools
      tmux
      keepass xdotool
      colordiff
      unzip
      spotify
      xclip
      yubikey-personalization-gui
      (callPackage ./apps/wesnoth.nix {})

      # xmonad
      dmenu
      trayer
      haskellPackages.xmobar
      xcompmgr
      networkmanagerapplet
      kde4.kmix

      # nix-related tools
      nix-repl
      strategoPackages.strategoxt # provides `pp-aterm` for printing .drv files

     ]
     ++
     pythonDevel;

     mkEnv = name: paths: buildEnv {
       name = "badi-" + name + "-packages";
       paths = paths;
     };

    in {
      badi = {
        dain = mkEnv "dain" allPkgs;
        fangorn = mkEnv "fangorn" (lib.lists.remove libreoffice allPkgs);
      };
    };
}
