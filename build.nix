{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "lexicon";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/phenguin/lexicon";
  license = stdenv.lib.licenses.mit;
}
