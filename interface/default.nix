{ mkDerivation,
  aeson,
  base,
  stdenv,
  text,
  wai,
  warp,
  servant }:
mkDerivation {
  pname = "wzuri-interface";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  executableHaskellDepends = [ base aeson wai warp text servant ];
  license = stdenv.lib.licenses.bsd3;
}
