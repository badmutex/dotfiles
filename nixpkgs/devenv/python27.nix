{ python2Full, python2Packages }:

let pypkgs = pkgs: with pkgs; [
      pip
      virtualenv
      ipython
      jupyter
      numpy
      scipy
      ipykernel
      matplotlib
      attrs
      jedi
      rope
      flake8
      importmagic
      autopep8
      yapf
      pep8
      pyflakes
    ];

in

python2Full.buildEnv.override {
  extraLibs = pypkgs python2Packages;
  ignoreCollisions = true;
}
