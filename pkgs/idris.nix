{ doCheck ? true
, gcc
, callPackage
, haskellPackages
, idrisPackages
}:

let
  test = callPackage ./test.nix { doCheck = doCheck; };
  typedriven = callPackage ./typedriven.nix { doCheck = doCheck; test = test; };
in

idrisPackages.with-packages(with idrisPackages; [
  gcc prelude base effects test typedriven
])
