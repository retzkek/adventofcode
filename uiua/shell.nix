{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  name="uiua";
  buildInputs = [
    uiua

    neovim
    tmux
  ];
}
