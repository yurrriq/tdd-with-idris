{ pkgs ? import <nixpkgs> {}
, date ? "2016-11-30"
, doCheck ? true
}:
let
  inherit (pkgs) lib gcc fetchFromGitHub haskellPackages idrisPackages;
  inherit (haskellPackages) idris;
  inherit (idrisPackages) build-idris-package with-packages;
  test = build-idris-package {
   name = "test-${date}";

   src = fetchFromGitHub {
     owner = "yurrriq";
     repo = "idris-testing";
     rev = "8033dc1";
     sha256 = "0hpisraaiag7rl1agm4y6qj9pva8m8kbvnlvy2y13xh005n71xaa";
   };

   buildPhase = "${idris}/bin/idris --build test.ipkg";
   doCheck = false;
   installPhase = "${idris}/bin/idris --install test.ipkg";
   propagatedBuildInputs = with idrisPackages; [ prelude base effects ];

   meta = with lib; {
     description = "Simple Testing Library";
     homepage =  "https://github.com/jfdm/idris-testing";
     license = licenses.bsd3;
     maintainers = [ "Jan de Muijnck-Hughes" ];
     inherit (idris.meta) platforms;
   };
 };
 typedriven = build-idris-package rec {
  pkg = "typedriven";
  name = pkg;
  version = date;

  src = ./.;
  buildPhase = "${idris}/bin/idris --build ${pkg}.ipkg";
  checkPhase = ''
    ${idris}/bin/idris --build test.ipkg
    ${idris}/bin/idris -i src --build bin/test.ipkg
    ./runtests
  '';
  installPhase = "${idris}/bin/idris --install ${pkg}.ipkg";
  propagatedBuildInputs = with idrisPackages; [ prelude base test ] ++ [ gcc ];

  meta = with lib; {
    description = ''
      Solutions to exercises in Type-Driven Development with Idris.
    '';
    homepage = "https://github.com/yurrriq/tdd-with-idris";
    license = licenses.bsd3;
    maintainers = with maintainers; [ yurrriq ];
    inherit (idris.meta) platforms;
  };
  };
in
{
  idris = with-packages(with idrisPackages; [
    prelude base effects test typedriven # contrib pruviloj
  ]);
}
