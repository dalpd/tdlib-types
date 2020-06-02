pkgs: with pkgs;
let
  language-tl = callPackage ./nix/language-tl.nix {};
  tdlib-gen = callPackage ./nix/tdlib-gen.nix {inherit language-tl;};
in
[
  language-tl
  tdlib-gen

  aeson
  polysemy
  polysemy-plugin
  base64-bytestring-type
]
