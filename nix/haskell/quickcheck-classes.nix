{ mkDerivation, aeson, base, fetchgit, prim-array, primitive
, QuickCheck, stdenv, transformers, vector
}:
mkDerivation {
  pname = "quickcheck-classes";
  version = "0.3";
  src = fetchgit {
    url = "https://github.com/andrewthad/quickcheck-classes.git";
    sha256 = "0zmamrailcplbp44xirs7fsxg7b1x38ds9zyk87dm4lcjpdvzz60";
    rev = "a102f2b5d40b5baf74fd5846d8c77c13f470bcc2";
  };
  libraryHaskellDepends = [
    aeson base prim-array primitive QuickCheck transformers
  ];
  testHaskellDepends = [ aeson base primitive QuickCheck vector ];
  homepage = "https://github.com/andrewthad/quickcheck-classes#readme";
  description = "QuickCheck common typeclasses";
  license = stdenv.lib.licenses.bsd3;
}
