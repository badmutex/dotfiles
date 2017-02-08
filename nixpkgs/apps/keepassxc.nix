{ stdenv, lib, fetchurl, cmake, libgcrypt, qt5, zlib, libmicrohttpd, libXtst, libgpgerror
, withHTTP ? false
, withAutoType ? false
, withYubikey ? false
}:

stdenv.mkDerivation rec {
  name = "keepassxc-${version}";
  version = "2.1.1";

  src = fetchurl {
    url = "https://github.com/keepassxreboot/keepassxc/releases/download/${version}/${name}-src.tar.xz";
    sha256 = "1028h23gcwv3dmq306l4n74mm9vvqx2n8gm9i877ki6z3qzhc2an";
  };

  buildInputs = [ cmake libgcrypt zlib qt5.full libXtst libmicrohttpd libgpgerror ];
  enableParallelBilding = true;

  cmakeFlags = with lib; strings.concatStringsSep " " [
    (optionalString withHTTP "-DWITH_XC_HTTP=ON")
    (optionalString withAutoType "-DWITH_XC_AUTOTYPE=ON")
    (optionalString withYubikey "-DWITH_XC_YUBIKEY=ON")
  ];

  meta = {
    description = "Fork of the keepassX password-manager with additional http-interface to allow browser-integration an use with plugins such as PasslFox (https://github.com/pfn/passifox). See also keepassX2.";
    homepage = https://github.com/keepassxreboot/keepassx;
    license = stdenv.lib.licenses.gpl2;
    maintainers = with stdenv.lib.maintainers; [ s1lvester jonafato ];
    platforms = with stdenv.lib.platforms; linux;
  };

}

