{ stdenv, fetchFromGitHub, makeQtWrapper
, cmake, libgcrypt, zlib, libmicrohttpd, libXtst, qtbase, qttools, libgpgerror
, withHTTP ? false
, withAutoType ? false
, withYubikey ? false
}:

with stdenv.lib;

stdenv.mkDerivation rec {
  name = "keepassx-community-${version}";
  version = "2.1.4";

  src = fetchFromGitHub {
    owner = "keepassxreboot";
    repo = "keepassxc";
    rev = "${version}";
    sha256 = "1znnw2xpv58x0rbpmm4y662377mbmcilhf8mhhjsz8vhahms33a8";
  };

  cmakeFlags = [
    (optionalString withHTTP "-DWITH_XC_HTTP=ON")
    (optionalString withAutoType "-DWITH_XC_AUTOTYPE=ON")
    (optionalString withYubikey "-DWITH_XC_YUBIKEY=ON")
  ];

  postFixup = ''
    wrapQtProgram $out/bin/keepassxc --suffix-each LD_LIBRARY_PATH ':' "${qtbase}/lib"
  '';

  buildInputs = [ cmake libgcrypt zlib qtbase qttools libXtst libmicrohttpd libgpgerror ];
  nativeBuildInputs = [ makeQtWrapper ];

  meta = {
    description = "Fork of the keepassX password-manager with additional http-interface to allow browser-integration an use with plugins such as PasslFox (https://github.com/pfn/passifox). See also keepassX2.";
    homepage = https://github.com/keepassxreboot/keepassxc;
    license = stdenv.lib.licenses.gpl2;
    maintainers = with stdenv.lib.maintainers; [ s1lvester jonafato ];
    platforms = with stdenv.lib.platforms; linux;
  };
}
