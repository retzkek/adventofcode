{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
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
