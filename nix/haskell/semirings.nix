{ mkDerivation, base, containers, fetchgit, hashable, log-domain
, primitive, stdenv, unordered-containers, vector
}:
mkDerivation {
  pname = "semirings";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/chessai/semirings.git";
    sha256 = "155km9q9h1j50n3qqqdn62f7znbi8pfvfjj29izn4y0lcs740vp6";
    rev = "ed0238862becf5ae459bc796c0f95def8aa034af";
  };
  libraryHaskellDepends = [
    base containers hashable log-domain primitive unordered-containers
    vector
  ];
  homepage = "https://github.com/chessai/semirings#readme";
  description = "semirings";
  license = stdenv.lib.licenses.bsd3;
}
