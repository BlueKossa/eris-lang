{
  description = "Eris Dev Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    devShells.${system}.default =
      pkgs.mkShell
        {
          buildInputs = [
            pkgs.libffi
            pkgs.libxml2
            pkgs.ncurses
            pkgs.llvmPackages_14.llvm.dev
          ];

          LLVM_SYS_140_PREFIX = pkgs.llvmPackages_14.llvm.dev;
        };
  };
}
