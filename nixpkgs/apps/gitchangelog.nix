{ buildPythonPackage, stdenv, fetchurl, d2to1, ... }:

buildPythonPackage rec {
  name = "gitchangelog-2.3.0";

  meta = {
    description = "gitchangelong generates a changelog thanks to git log.";
    homepage = "https://pypi.python.org/pypi/gitchangelog";
    license = stdenv.lib.licenses.bsd;
  };

  src = fetchurl {
    url = "mirror://pypi/g/gitchangelog/${name}.tar.gz";
    sha256 = "01h0ajn2ycmqfa2kcgbyq2lsrp03v88hddrivw9whp33h15s187k";
  };

  buildInputs = [ d2to1 ];

}
