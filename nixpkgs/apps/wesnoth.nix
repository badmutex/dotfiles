{ stdenv, fetchurl, cmake, SDL, SDL_image, SDL_mixer, SDL_net, SDL_ttf, pango
, gettext, zlib, boost, freetype, libpng, pkgconfig, lua, dbus, fontconfig, libtool
, fribidi, asciidoc, libvorbis }:

stdenv.mkDerivation rec {
  pname = "wesnoth";
  version = "1.12.2";

  name = "${pname}-${version}";

  src = fetchurl {
    url = "mirror://sourceforge/sourceforge/${pname}/${name}.tar.bz2";
    sha256 = "1f4f76e5fd0ce175a3eb7b9855aff7a58dc75899c534d7653d97ac9fd4fe798b";
  };

  buildInputs = [ SDL SDL_image SDL_mixer SDL_net SDL_ttf pango gettext zlib
                  boost fribidi cmake freetype libpng pkgconfig lua
                  dbus fontconfig libtool libvorbis ];

  cmakeFlags = [ "-DENABLE_STRICT_COMPILATION=FALSE" ]; # newer gcc problems http://gna.org/bugs/?21030

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    description = "The Battle for Wesnoth, a free, turn-based strategy game with a fantasy theme";
    longDescription = ''
      The Battle for Wesnoth is a Free, turn-based tactical strategy
      game with a high fantasy theme, featuring both single-player, and
      online/hotseat multiplayer combat. Fight a desperate battle to
      reclaim the throne of Wesnoth, or take hand in any number of other
      adventures.
    '';

    homepage = http://www.wesnoth.org/;
    license = licenses.gpl2;
    maintainers = [ maintainers.kkallio ];
    platforms = platforms.linux;
  };
}
