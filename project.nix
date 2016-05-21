{ inputModifier ? x: x }:
{ mkDerivation, base, containers, criterion, free, parsec
, QuickCheck, stdenv, text, transformers, unordered-containers, data-fix
}:
mkDerivation (inputModifier {
  pname = "lexicon";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers criterion free parsec QuickCheck text transformers
    unordered-containers data-fix
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/phenguin/lexicon";
  license = stdenv.lib.licenses.mit;
})
