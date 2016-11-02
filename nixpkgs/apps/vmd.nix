{ requireFile, stdenv, makeWrapper, which, perl , gnutar, gnumake,
gcc, xlibs, mesa, mesa_glu
, ...}:

let

  shortname = "vmd";
  version = "1.9.3beta3";

  deps = [
    gcc.cc
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
    name = "vmd-1.9.3beta3.bin.LINUXAMD64-OptiX.opengl.tar.gz";
    sha256 = "032174926b24a21fd7d777dc72631b7295998d1ebf4f3b05a95ea95f62eef2c5";
    url = "http://www.ks.uiuc.edu/Research/vmd";
  };


  buildInputs = [ makeWrapper which perl gnutar gnumake xlibs.libX11 xlibs.libXinerama ];

  phases = [ "unpackPhase" "installPhase" ];  

  unpackPhase = "true";

  installPhase = ''
    tar xf $src
    cd vmd-${version}
    echo "out=$out"

    export VMDINSTALLBINDIR=$out/bin
    export VMDINSTALLLIBRARYDIR=$out/lib/vmd
    echo LINUXAMD64 OPENGL OPENGLPBUFFER FLTK TK XINERAMA XINPUT TCL PYTHON PTHREADS > configure.options
    perl ./configure

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
