{ mkDerivation, base, data-default, wzuri-interface, reflex, reflex-dom, servant, servant-reflex, stdenv, text }:
mkDerivation {
  pname = "frontend";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base data-default wzuri-interface reflex reflex-dom servant servant-reflex text ];
  license = stdenv.lib.licenses.bsd3;
}
