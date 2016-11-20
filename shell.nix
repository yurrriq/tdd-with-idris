let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in stdenv.mkDerivation {
  name = "typedriven";
  src = ./.;
  buildInputs = with pkgs; [ haskellPackages.idris gcc gmp libffi ];
}
