{ date ? "2016-11-30"
, doCheck ? true
, lib
, fetchFromGitHub
, haskellPackages
, idris ? haskellPackages.idris
, idrisPackages
}:

idrisPackages.build-idris-package {
 name = "test-${date}";

 src = fetchFromGitHub {
   owner = "yurrriq";
   repo = "idris-testing";
   rev = "8033dc1";
   sha256 = "0hpisraaiag7rl1agm4y6qj9pva8m8kbvnlvy2y13xh005n71xaa";
 };

 buildPhase = "${idris}/bin/idris --build test.ipkg";

 doCheck = doCheck;

 checkPhase = ''
   ${idris}/bin/idris --checkpkg test.ipkg
   # NOTE: requires lightyear
   # ${idris}/bin/idris --checkpkg testparse.ipkg
 '';

 installPhase = "${idris}/bin/idris --install test.ipkg";

 propagatedBuildInputs = with idrisPackages; [ prelude base effects ];

 meta = with lib; {
   description = "Simple Testing Library";
   homepage =  "https://github.com/jfdm/idris-testing";
   license = licenses.bsd3;
   maintainers = [ "Jan de Muijnck-Hughes" ];
   inherit (idris.meta) platforms;
 };
}
