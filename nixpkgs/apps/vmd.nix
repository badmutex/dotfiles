{ requireFile, stdenv, makeWrapper, which, perl , gnutar, gnumake,
gcc, xlibs, mesa, mesa_glu
, ...}:

let

  shortname = "vmd";
  version = "1.9.2";

  deps = [
    gcc.gcc
    mesa
    mesa_glu
    xlibs.libICE
    xlibs.libSM
    xlibs.libX11
    xlibs.libXinerama
    xlibs.libXi
  ];


in

stdenv.mkDerivation {
  name = "${shortname}-${version}";

  src = requireFile {
    name = "vmd-1.9.2.bin.LINUXAMD64-RHEL5.opengl.tar.gz";
    sha256 = "c7a9520abe0526f90a74c8a1b699a83f4b3cca4446c70b627af1ba3b599aad19";
    url = "http://www.ks.uiuc.edu/Research/vmd";
  };


  buildInputs = [ makeWrapper which perl gnutar gnumake ];

  unpackPhase = "true";

  installPhase = ''
    tar xf $src
    cd vmd-${version}
    echo "out=$out"

    export VMDINSTALLBINDIR=$out/bin
    export VMDINSTALLLIBRARYDIR=$out/lib/vmd
    ./configure

    cd src
    make install

    interpreter=$(patchelf --print-interpreter $(which patchelf))
    patchelf --set-interpreter $interpreter $out/lib/vmd/vmd_LINUXAMD64

    librarypath="${stdenv.lib.makeLibraryPath deps}:$out/lib/vmd"
    wrapProgram $out/bin/vmd --prefix LD_LIBRARY_PATH : "$librarypath"
  '';

  dontStrip = true;
  dontPatchElf = true;

}