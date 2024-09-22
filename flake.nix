{
  description = "A Nix flake for Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }: let
    
    system = "x86_64-linux";

  in {
    devShells."${system}".default = let
      pkgs = import nixpkgs {
        inherit system;
      };

    in pkgs.mkShell {
      packages = with pkgs; [
        cabal-install
        stack
        haskell.compiler.ghc8107
        haskell-language-server
      ];

      shellHook = ''
        echo "ghc `${pkgs.haskell.compiler.ghc8107}/bin/ghc --version`"
      '';
    };
  };
}
