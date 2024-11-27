{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    sbcl
    rlwrap
    asdf
    openssl.out

    #julia

    elixir

    jdk
    clojure
    babashka
    neil
    clojure-lsp
    clj-kondo

    emacs
    neovim
    tmux
  ];
}
