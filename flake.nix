{
  description = "Crossfire - a Connect-4 variant built with Miso";

  nixConfig = {
    extra-substituters      = [ "https://haskell-miso-cachix.cachix.org" ];
    extra-trusted-public-keys = [
      "haskell-miso-cachix.cachix.org-1:m8hN1cvFMJtYib4tj+06xkKt5ABMSGfe8W7s40x1kQ0="
    ];
  };

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs?rev=9e2e8a7878573d312db421d69e071690ec34e98c";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs      = nixpkgs.legacyPackages.${system};
        ghcjsPkgs = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122;

        crossfire = ghcjsPkgs.callCabal2nix "crossfire" ./. {};
      in
      {
        packages.default = crossfire;
      }
    );
}
