{ mkDerivation, aeson, array, attoparsec, base, bytestring
, containers, deepseq, directory, dlist, ghc-prim, ghcjs-prim
, hashable, HUnit, integer-gmp, primitive, QuickCheck
, quickcheck-unicode, random, scientific, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, time
, transformers, unordered-containers, vector, fetchFromGitHub
}:
mkDerivation {
  pname = "ghcjs-base";
  version = "0.2.0.0";
  src = fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-base";
    rev = "eacf95aac3061275699563e1802eabe4a8f4aaec";
    sha256 = "096xdjp53kg1cpmc6qncjfhd8fwnbmjagpxzdd5w101pkiknd1x6";
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers deepseq dlist ghc-prim
    ghcjs-prim hashable integer-gmp primitive scientific text time
    transformers unordered-containers vector
  ];
  testHaskellDepends = [
    array base bytestring deepseq directory ghc-prim ghcjs-prim HUnit
    primitive QuickCheck quickcheck-unicode random test-framework
    test-framework-hunit test-framework-quickcheck2 text
  ];
  homepage = "http://github.com/ghcjs/ghcjs-base";
  description = "base library for GHCJS";
  license = stdenv.lib.licenses.mit;
}
