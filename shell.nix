with (import <nixpkgs> {});
pkgs.stdenv.mkDerivation {
  name = "tdd-with-idris-env-0";
  buildInputs = with pkgs; [
    gcc gmp libffi
    haskellPackages.idris
  ];
}
