{
  description = "Haskell Dev Shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
    in
    {
      devShells.${system}.default = pkgs.mkShell.override { } {
        packages = with pkgs; [
          nixd
          nil
          package-version-server

          haskellPackages.ghc
          pkgs.haskell-language-server
          pkgs.ormolu
        ];

        shellHook = ''
          echo "======== Haskell Dev Shell ========"
          echo "GHC Version: $(ghc --version)"
        '';
      };

    };
}
