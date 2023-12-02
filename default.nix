{ pkgs ? import <nixpkgs> {} }:

{
env = pkgs.buildEnv {
  name = "my-packages";
  paths = with pkgs; [
    coreutils
    gcc
    emacs
    curl
    git
    sbcl
    openssl.out
  ];
  pathsToLink = [ "/share" "/bin" "/lib" "/include"];
  extraOutputsToInstall = [ "lib" ];
  };
}
