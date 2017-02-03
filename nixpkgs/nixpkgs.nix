

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

    "2017-01-12" = {
      rev = "60435691f78dc533d49bf30751e1a8328b67367a";
      sha256 = "14404p4k5vsl7crjsd03vcbgymvlwyxj86mmbbcj9hcjdnadh8iw";
    };

    "2017-01-18" = {
      rev = "460b43dbfe8f21253637be350eb1ef1d586eaf5e";
      sha256 = "19hkwa3yx33z42jprfkdcdabcdrg13cjwwf7s36fzn8i9rlp8bls";
    };

    "2017-01-22" = {
      rev = "cb602a472885f0b32c917b73ec1e5353f8e5e450";
      sha256 = "178xwvb9qjw43da2frd8q5zmli2zm2rxm0vbx68wv8myhhlmpwgj";
    };


  };

  origin = hashes."2017-01-22" // {
    owner = "NixOS";
    repo = "nixpkgs-channels";
  };

  src = pkgs.fetchFromGitHub origin;

in

import src {}
