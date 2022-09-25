{ pkgs ? import <nixpkgs> {}, easy-ps }:

pkgs.mkShell {
  buildInputs = [
    pkgs.nodejs-16_x
    easy-ps.purs-0_15_4
    easy-ps.purs-tidy
    easy-ps.psc-package
    easy-ps.spago
    easy-ps.pscid
  ];
}
