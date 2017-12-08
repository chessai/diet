{ mkDerivation, aeson, base, fetchgit, prim-array, primitive
, QuickCheck, stdenv
}:
mkDerivation {
  pname = "quickcheck-classes";
  version = "0.2";
  src = fetchgit {
    url = "https://github.com/andrewthad/quickcheck-classes.git";
    sha256 = "0phgskq6nwkyq866sg8zk836kvmc8np2isask5dpan3xxfxc4b41";
    rev = "a9d07981fd9912d56cfcbaccc1208f740e3a02ec";
  };
  libraryHaskellDepends = [
    aeson base prim-array primitive QuickCheck
  ];
  testHaskellDepends = [ aeson base primitive QuickCheck ];
  homepage = "https://github.com/andrewthad/quickcheck-classes#readme";
  description = "QuickCheck common typeclasses";
  license = stdenv.lib.licenses.bsd3;
}
