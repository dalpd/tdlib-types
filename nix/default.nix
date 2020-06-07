{ compiler }:
let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };

  inherit (import sources.gitignore { inherit (pkgs) lib; }) gitignoreSource;

  hPkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      "tdlib-types" =
        self.callCabal2nix "tdlib-types" (gitignoreSource ../.) { };
    };
  };

  shell = hPkgs.shellFor {
    packages = p: [ p."tdlib-types" ];

    nativeBuildInputs = with pkgs.haskellPackages; [
      cabal-install
      ormolu
      hlint
    ] ++ [
      hPkgs.ghcide
    ];
  };

  lib = hPkgs."tdlib-types".overrideAttrs (_: {
    configureFlags = [
      "--enable-tests"
      "--enable-optimization"
      "--enable-static"
      "--enable-shared"
      "--enable-profiling"
    ];
  });
in { inherit shell lib; }
