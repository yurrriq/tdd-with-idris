{ pkgs ? import <nixpkgs> {}
, date ? "2016-11-22"
, version ? date
, doCheck ? true
}:
let
  inherit (pkgs) lib gcc fetchFromGitHub haskellPackages idrisPackages;
  inherit (haskellPackages) idris;
  inherit (idrisPackages) build-idris-package with-packages;
  test = build-idris-package {
   name = "test-${date}";

   src = fetchFromGitHub {
     owner = "jfdm";
     repo = "idris-testing";
     rev = "24a75bb71350fd64b00b72f69daa93530b49e61a";
     sha256 = "1k2qqz9vn9h8g4zxslx61z2g9ks5y29y3fmrlv7vawymkhcjrcl5";
   };

   buildPhase = "${idris}/bin/idris --build test.ipkg";
   doCheck = false;
   installPhase = "${idris}/bin/idris --install test.ipkg";
   propagatedBuildInputs = with idrisPackages; [ prelude base effects ];

   meta = with lib; {
     description = "Parser combinators for Idris";
     homepage =  "https://github.com/jfdm/idris-testing";
     license = licenses.bsd3;
     maintainers = [ "Jan de Muijnck-Hughes" ];
     inherit (idris.meta) platforms;
   };
 };
 version = date;
 pkgName = "typedriven";
 typedriven = build-idris-package {
  name = pkgName;
  version = version;

  src = ./.;
  buildPhase = "${idris}/bin/idris --build ${pkgName}.ipkg";
  checkPhase = "${idris}/bin/idris --testpkg test.ipkg";
  doCheck = doCheck;
  installPhase = "${idris}/bin/idris --install ${pkgName}.ipkg";
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
    prelude base contrib effects pruviloj test typedriven
  ]);
}
