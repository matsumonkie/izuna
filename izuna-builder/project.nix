{ mkDerivation, aeson, array, async, base, bytestring, containers
, directory, filepath, foreign-store, generic-lens, ghc-lib
, ghc-lib-parser, ghc-paths, hpack, hspec, html-entities, mtl
, safe-exceptions, say, servant, servant-multipart, servant-server
, stdenv, tar, text, wai, warp, ghcVersion
}:
mkDerivation {
  pname = "izuna-builder-${ghcVersion}";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  doHaddock = false;
  libraryHaskellDepends = [
    aeson array async base bytestring containers directory filepath
    foreign-store generic-lens ghc-lib ghc-lib-parser ghc-paths
    html-entities mtl safe-exceptions say servant servant-multipart
    servant-server tar text wai warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base containers hspec text ];
  prePatch = "hpack";
  homepage = "https://github.com/matsumonkie/izuna#readme";
  license = stdenv.lib.licenses.bsd3;
}
