{ date ? "2016-11-30"
, doCheck ? true
, lib
, gcc
, haskellPackages
, idris ? haskellPackages.idris
, idrisPackages
, test
}:

let
  pkg = "typedriven";
in

idrisPackages.build-idris-package {
  pkg = pkg;
  name = pkg;
  version = date;

  src = ./..;

  buildPhase = ''
    ${idris}/bin/idris --build ${pkg}.ipkg
  '';

  doCheck = doCheck;

  checkPhase = ''
    ${idris}/bin/idris --build test.ipkg
    ${idris}/bin/idris -i src --build bin/test.ipkg
    ./runtests
  '';

  installPhase = "${idris}/bin/idris --install ${pkg}.ipkg";

  propagatedBuildInputs = with idrisPackages; [ gcc prelude base test ];

  meta = with lib; {
    description = ''
      Solutions to exercises in Type-Driven Development with Idris.
    '';
    homepage = "https://github.com/yurrriq/tdd-with-idris";
    license = licenses.bsd3;
    maintainers = with maintainers; [ yurrriq ];
    inherit (idris.meta) platforms;
  };
}
