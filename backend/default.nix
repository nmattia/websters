{ mkDerivation, base, aeson, wzuri-interface, stdenv, wai, warp, servant-server }:
mkDerivation {
  pname = "wzuri-backend";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base aeson wzuri-interface wai warp servant-server ];
  license = stdenv.lib.licenses.bsd3;
}
