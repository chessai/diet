{ mkDerivation, base, discrete-intervals, QuickCheck
, quickcheck-classes, stdenv
}:
mkDerivation {
  pname = "diet";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base discrete-intervals ];
  testHaskellDepends = [ base QuickCheck quickcheck-classes ];
  homepage = "http://github.com/chessai/diet";
  description = "Discrete Interval Encoding Tree";
  license = stdenv.lib.licenses.bsd3;
}
