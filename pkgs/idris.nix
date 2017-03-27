{ doCheck ? true
, gcc
, libffi
, callPackage
, haskellPackages
, idrisPackages
}:

let
  test = callPackage ./test.nix { inherit doCheck; };
  typedriven = callPackage ./typedriven.nix { inherit doCheck test; };
in

idrisPackages.with-packages(with idrisPackages; [
  gcc libffi
  prelude base effects
  test typedriven
])
