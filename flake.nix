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
        # Patch emscripten to always export HEAP8 etc., which GHCJS depends on
        # but Emscripten 4.x stopped exporting by default.
        pkgs = nixpkgs.legacyPackages.${system}.extend (self: super: {
          emscripten = super.emscripten.overrideAttrs (old: {
            postFixup = (old.postFixup or "") + ''
              orig=$out/bin/.emcc-original
              mv $out/bin/emcc $orig
              cat > $out/bin/emcc << EOF
#!/bin/sh
exec $orig -sEXPORTED_RUNTIME_METHODS=HEAP8,HEAP16,HEAP32,HEAPU8,HEAPU16,HEAPU32,HEAPF32,HEAPF64,getTempRet0,setTempRet0 "\$@"
EOF
              chmod +x $out/bin/emcc
            '';
          });
        });

        ghcjsPkgs = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122;
        crossfire  = ghcjsPkgs.callCabal2nix "crossfire" ./. {};
      in
      {
        packages.default = crossfire;
      }
    );
}
