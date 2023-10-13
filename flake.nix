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
          # Idk
          shellHook = ''
            session="eris-dev"
            sessionExists=$(tmux list-sessions | grep $session)

            if [ "$sessionExists" != "" ]; then
              tmux kill-session -t $session
            fi

            tmux new-session -d -s $session
            tmux set -g mouse on
            tmux set -g mouse-select-window on

            tmux send-keys -t $session:1 "nvim src/main.rs" C-m
            tmux rename-window -t $session:1 "nvim"
            
            tmux new-window -t $session -n "shell"
            tmux attach-session -t $session:1
            exit
          '';

          LLVM_SYS_140_PREFIX = pkgs.llvmPackages_14.llvm.dev;
        };
  };
}
