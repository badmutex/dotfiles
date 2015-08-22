{ pkgs
, ...
}:

{
  mkEnv = paths: pkgs.buildEnv {
    name = "badi-packages";
    paths = paths;
  };        
}
