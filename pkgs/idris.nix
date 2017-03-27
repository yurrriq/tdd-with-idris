{ doCheck ? true
, gcc
, libffi
, callPackage
, haskellPackages
, idrisPackages
}:

let
  test = callPackage ./test.nix { doCheck = doCheck; };
  typedriven = callPackage ./typedriven.nix { doCheck = doCheck; test = test; };
in

idrisPackages.with-packages(with idrisPackages; [
  gcc libffi
  prelude base effects
  test typedriven
])
