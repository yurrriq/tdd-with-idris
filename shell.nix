{ doCheck ? false }:

with import <nixpkgs> {};

let
  idris = callPackage ./pkgs/idris.nix { inherit doCheck; };
in

stdenv.mkDerivation rec {
  name = "tdd-with-idris-${version}";
  version = "0.0.1";

  buildInputs = [ idris ];
}
