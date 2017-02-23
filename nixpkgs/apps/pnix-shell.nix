{ stdenv
, fetchurl
, gnused
, bash
, ...
}:

let

  gist = {
  };

  fetchGist =
    { user, id, sha, name, sha256}:
    fetchurl {
      url = "https://gist.githubusercontent.com/${user}/${id}/raw/${sha}/${name}";
      inherit sha256;
    };

in

stdenv.mkDerivation rec {

  revision = "3";
  pname = "pnix-shell";
  name = "${pname}-${revision}";

  buildInputs = [ gnused ];
  propagatedBuildInptus = [ bash ];

  src = fetchGist {
    user = "aherrmann";
    name = "${pname}.sh";
    id = "51b56283f9ed5853747908fbab907316";
    sha = "a5f4390e7aa587ce5b5bb6acb6435aa066c0daf7";
    sha256 = "0mvnlnl6cx0gszkpayd1y5ighmsm3di3d9mml83v14ka9p72zwmj";
  };

  phases = [ "installPhase" ];

  installPhase = ''
    declare -r bin=$out/bin
    declare -r exe=$bin/${pname}
    mkdir -p $bin
    cp $src $exe
    chmod +x $exe
    
    # add shebang
    sed -i '1i #!${bash}/bin/bash' $exe

    # don't require sourcing
    sed -i '$s/$/\npnix-shell $@/' $exe
  '';

}
