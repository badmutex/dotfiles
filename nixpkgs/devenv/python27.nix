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
      virtualenvwrapper
      jedi
      pyflakes
      pep8
      autopep8
    ]
    ;
}
