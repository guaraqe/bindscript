{ mkDerivation, base, directory, directory-tree, filepath
, megaparsec, optparse-generic, prettyprinter, stdenv, tasty
, tasty-hunit, text
}:
mkDerivation {
  pname = "bindscript";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec prettyprinter text ];
  executableHaskellDepends = [
    base directory directory-tree filepath megaparsec optparse-generic
    text
  ];
  testHaskellDepends = [ base megaparsec tasty tasty-hunit ];
  description = "Synopsis";
  license = stdenv.lib.licenses.bsd3;
}
