let
  extras = import ./nix/extras.nix;
  pkgs = extras.pinnedPkgs {
    specFile = ./nix/nixpkgs.json;
    opts = {};
  };
in
  extras.purescriptDevEnv { inherit pkgs; }
