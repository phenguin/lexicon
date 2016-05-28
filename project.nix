{ inputModifier ? x: x }:
{ mkDerivation, base, containers, criterion, data-fix, free, parsec
, QuickCheck, stdenv, text, transformers, unordered-containers
}:
mkDerivation (inputModifier {
  pname = "lexicon";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers criterion data-fix free parsec QuickCheck text
    transformers unordered-containers
  ];
  executableHaskellDepends = [ base text ];
  homepage = "https://github.com/phenguin/lexicon";
  license = stdenv.lib.licenses.mit;
})
