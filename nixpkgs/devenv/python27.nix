{ pkgs, python27Packages }:

pkgs.buildEnv {
  name = "python27-devenv";
  paths =
    with python27Packages;
    [
      pkgs.python27Full
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
