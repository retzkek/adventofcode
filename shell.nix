{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    sbcl
    rlwrap
    asdf
    openssl.out

    julia

  ];
}
