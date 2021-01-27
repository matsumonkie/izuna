{ mkDerivation, array, base, bytestring, directory, http-client
, http-media, http-types, lens, network, random, resourcet, servant
, servant-client, servant-client-core, servant-docs
, servant-foreign, servant-server, stdenv, string-conversions
, tasty, tasty-wai, text, transformers, wai, wai-extra, warp
}:
mkDerivation {
  pname = "servant-multipart";
  version = "0.12";
  sha256 = "74e398d14426e077105b4da57e980362392d1a6025b615be757642f98d8141c3";
  revision = "1";
  editedCabalFile = "0pc254b458v4k5xw729fvw3q3klwpkai2mmp455dw2i7g02dv0da";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bytestring directory http-media lens random resourcet
    servant servant-client-core servant-docs servant-foreign
    servant-server string-conversions text transformers wai wai-extra
  ];
  executableHaskellDepends = [
    base bytestring http-client network servant servant-client
    servant-client-core servant-server text transformers wai warp
  ];
  testHaskellDepends = [
    base bytestring http-types servant-server string-conversions tasty
    tasty-wai text
  ];
  homepage = "https://github.com/haskell-servant/servant-multipart#readme";
  description = "multipart/form-data (e.g file upload) support for servant";
  license = stdenv.lib.licenses.bsd3;
}
