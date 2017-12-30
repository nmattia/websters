{ mkDerivation, base, bifunctors, comonad, containers, criterion
, data-default, deepseq, dependent-map, dependent-sum, directory
, exception-transformers, filemanip, filepath, ghcjs-base
, haskell-src-exts, haskell-src-meta, hlint, lens, loch-th
, MemoTrie, monad-control, mtl, prim-uniq, primitive, process
, random, ref-tf, reflection, semigroupoids, semigroups, split
, stdenv, stm, syb, template-haskell, these, time, transformers
, transformers-compat, unbounded-delays, fetchFromGitHub
}:
mkDerivation {
  pname = "reflex";
  version = "0.5.0";
  src = fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex";
    rev = "d9ef8457dfd140fde32d15a6959a15ddcd7e15e4";
    sha256 = "0946817b2q1jkw50hb4fm04c4siq94di2y042ajrdbaqz0qlq6cm";
  };
  libraryHaskellDepends = [
    base bifunctors comonad containers data-default dependent-map
    dependent-sum exception-transformers ghcjs-base haskell-src-exts
    haskell-src-meta lens MemoTrie monad-control mtl prim-uniq
    primitive random ref-tf reflection semigroupoids semigroups stm syb
    template-haskell these time transformers transformers-compat
    unbounded-delays
  ];
  testHaskellDepends = [
    base bifunctors containers deepseq dependent-map dependent-sum
    directory filemanip filepath hlint lens mtl ref-tf semigroups split
    these transformers
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq dependent-map dependent-sum
    loch-th mtl primitive process ref-tf split stm time transformers
  ];
  homepage = "https://github.com/reflex-frp/reflex";
  description = "Higher-order Functional Reactive Programming";
  license = stdenv.lib.licenses.bsd3;
}
