let
  bootstrap = import <nixpkgs> { };
  inherit (bootstrap) fetchFromGitHub;
  loadJSON = file: builtins.fromJSON (builtins.readFile file);
  sources = {
    nixpkgs = loadJSON ./nixpkgs.json;
    gitignore = loadJSON ./gitignore.json;
    language-tl = loadJSON ./language-tl.json;
    tdlib-gen = loadJSON ./tdlib-gen.json;
  };
in builtins.mapAttrs (_: fetchFromGitHub) sources
