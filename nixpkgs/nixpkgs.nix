

let

  pkgs = import <nixpkgs> {};

  # to get the new set of hashes, run
  # nix-prefetch-git git://github.com/NixOS/nixpkgs-channels refs/heads/nixpkgs-unstable
  hashes = {

    "2016-12-05" = {
      rev = "497e6d2f1d149f5fbe004e15fe8c384eed993943";
      sha256 = "1mh1pmgfi6xrfhx3c7r47iip06w66r9xzqwda79z73hmgq5zbvhx";
    };

    "2017-01-02" = {
      rev = "1fa75a5bb7cdb9f2413d8b20726ce69523bfe4c6";
      sha256 = "1m20qfgqnbbxzxdkf5x5kvhj996zw6pkikzsyckghxcpwh38ipcv";
    };

    "2017-01-09" = {
      rev = "50ec3fe1ac93d6059f67396fc7954c17084a1b20";
      sha256 = "04iazr7r744vf0pkaa3wyzprd90dwisfi0rfymh05xpjvmff4w86";
    };

  };

  origin = hashes."2017-01-09" // {
    owner = "NixOS";
    repo = "nixpkgs-channels";
  };

  src = pkgs.fetchFromGitHub origin;

in

import src {}
