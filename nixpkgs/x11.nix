{ pkgs, stdenv,
}:

if ! stdenv.isLinux then []
else with pkgs; [
  spotify
  feh
  gtk
  gtk-engine-murrine
  gtk_engines
  oxygen_gtk
  lxappearance
  gnome.gnomeicontheme
  hicolor_icon_theme
]
