let
  pkgs = import (fetchTarball("channel:nixpkgs-unstable")) {};
in pkgs.mkShell {
  buildInputs = [
    pkgs.libffi
    pkgs.libxml2
    pkgs.ncurses
    pkgs.llvmPackages_14.llvm.dev
  ];
  LLVM_SYS_140_PREFIX = "${pkgs.llvmPackages_14.llvm.dev}";
}
