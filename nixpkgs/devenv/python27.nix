{ pkgs, python2Packages }:

pkgs.buildEnv {
  name = "python2-devenv";
  paths =
    with python2Packages;
    [
      pkgs.python2Full
      ipython
      pip
      virtualenv
      # virtualenvwrapper
      jedi
      flake8
      importmagic
      autopep8
      yapf
      pep8
      pyflakes
    ]
    ;
}
