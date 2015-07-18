{

  allowUnfree = true;

  packageOverrides = pkgs: with pkgs;
    let

      pythonDevel = with python27Packages; [
        pythonFull
        jedi
        pyflakes
        pip
        virtualenv
      ];

       compression = [
         unzipNLS
         zip
       ];

       web = [
         chromium
       ];

       editors = [
         emacs
       ];

       office = [
         evince
         kde4.gwenview
         # libreoffice
         inkscape
         aspell
         aspellDicts.en
         thunderbird
       ];

       latex = [
         texLiveFull
         biber
         texmaker
       ];

       password_management = [
         keepass
         xdotool
         (callPackage ./apps/yubikey-personalization-gui.nix { qt=qt4; })
       ];

       games = [
         (callPackage ./apps/wesnoth.nix {})
       ];

       x11 = [
         terminator
         synergy
         feh
         xclip
         spotify

         gtk
         gtk-engine-murrine
         gtk_engines
         oxygen_gtk
         lxappearance
         gnome.gnomeicontheme
         hicolor_icon_theme
       ];

       xmonad = [
         dmenu
         trayer
         xcompmgr
         kde4.kmix
         haskellPackages.xmobar
         networkmanagerapplet
       ];

       tools = [
         colordiff
       ];

       mkEnv = name: paths: buildEnv {
         name = "badi-" + name + "-packages";
         paths = paths;
       };

    in {
      badi = {
        fangorn = mkEnv "fangorn" (
          pythonDevel ++
          compression ++ web ++ editors ++ office ++ tools ++
          password_management ++ games ++ x11 ++ xmonad
        );
      };
    };
}
