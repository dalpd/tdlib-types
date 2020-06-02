{ mkDerivation, aeson, base, bytestring, hpack, polysemy, stdenv }:
mkDerivation {
  pname = "tdlib-polysemy";
  version = "0.1.0";
  src = ../../tdlib-polysemy;
  libraryHaskellDepends = [ aeson base bytestring polysemy ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [ aeson base bytestring polysemy ];
  prePatch = "hpack";
  homepage = "https://github.com/poscat0x04/tdlib-polysemy#readme";
  license = stdenv.lib.licenses.bsd3;
}
