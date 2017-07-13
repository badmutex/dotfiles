{ stdenv, fetchFromGitHub, makeQtWrapper
, cmake, libgcrypt, zlib, libmicrohttpd, libXtst, qtbase, qttools, qtx11extras, libgpgerror
, withHTTP ? false
, withAutoType ? false
, withYubikey ? false
}:

with stdenv.lib;

stdenv.mkDerivation rec {
  name = "keepassx-community-${version}";
  version = "2.1.3";

  src = fetchFromGitHub {
    owner = "keepassxreboot";
    repo = "keepassxc";
    rev = "${version}";
    sha256 = "1zamk3dc44fn61b880i3l1r0np2sx2hs05cvcf2x4748r3xicacf";
  };

# CMake Warning at src/autotype/CMakeLists.txt:4 (find_package):
#   By not providing "FindQt5X11Extras.cmake" in CMAKE_MODULE_PATH this project
#   has asked CMake to find a package configuration file provided by
#   "Qt5X11Extras", but CMake did not find one.

#   Could not find a package configuration file provided by "Qt5X11Extras"
#   (requested version 5.2) with any of the following names:

#     Qt5X11ExtrasConfig.cmake
#     qt5x11extras-config.cmake

#   Add the installation prefix of "Qt5X11Extras" to CMAKE_PREFIX_PATH or set
#   "Qt5X11Extras_DIR" to a directory containing one of the above files.  If
#   "Qt5X11Extras" provides a separate development package or SDK, be sure it
#   has been installed.


  dontStrip = true;
  cmakeFlags = [
    "-DCMAKE_BUILD_TYPE=DEBUGFULL"
    "-DCMAKE_CXX_FLAGS_DEBUG='-g2'"
    "-DCMAKE_C_FLAGS_DEBUG='-g2'"
    (optionalString withHTTP "-DWITH_XC_HTTP=ON")
    (optionalString withAutoType "-DWITH_XC_AUTOTYPE=ON")
    (optionalString withYubikey "-DWITH_XC_YUBIKEY=ON")
  ];

  postFixup = ''
    wrapQtProgram $out/bin/keepassxc --suffix-each LD_LIBRARY_PATH ':' "${qtbase}/lib"
  '';

  buildInputs = [ cmake libgcrypt zlib qtbase qttools qtx11extras libXtst libmicrohttpd libgpgerror ];
  nativeBuildInputs = [ makeQtWrapper ];

  meta = {
    description = "Fork of the keepassX password-manager with additional http-interface to allow browser-integration an use with plugins such as PasslFox (https://github.com/pfn/passifox). See also keepassX2.";
    homepage = https://github.com/keepassxreboot/keepassxc;
    license = stdenv.lib.licenses.gpl2;
    maintainers = with stdenv.lib.maintainers; [ s1lvester jonafato ];
    platforms = with stdenv.lib.platforms; linux;
  };
}
