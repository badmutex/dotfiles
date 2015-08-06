{ pkgs, python27Packages }:

pkgs.buildEnv {
  name = "python27-devenv";
  paths =
    with python27Packages;
    [
      python
      pip
      virtualenv
      jedi
      pyflakes
      pep8
      autopep8
    ]
    ;
}
