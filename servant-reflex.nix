{ mkDerivation, aeson, base, bytestring, case-insensitive
, containers, data-default, exceptions, ghcjs-dom, http-api-data
, http-media, jsaddle, mtl, network-uri, reflex, reflex-dom, safe
, scientific, servant, servant-auth, stdenv, string-conversions
, text, transformers, fetchFromGitHub
}:
mkDerivation {
  pname = "servant-reflex";
  version = "0.3.3";
  src = fetchFromGitHub {
    owner = "nmattia";
    repo = "servant-reflex";
    rev = "4f77a91a35ddd89c0ac2ecefb1c1e115ad86c460";
    sha256 = "100ga6sd316zx07l2dxn269kxa0b16xsir08wv2h7y93apiraabj";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers data-default exceptions
    ghcjs-dom http-api-data http-media jsaddle mtl network-uri reflex
    reflex-dom safe servant servant-auth string-conversions text
    transformers
  ];
  executableHaskellDepends = [
    aeson base reflex reflex-dom scientific servant text
  ];
  description = "Servant reflex API generator";
  license = stdenv.lib.licenses.bsd3;
}
