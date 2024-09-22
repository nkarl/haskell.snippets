{
  description = "A Nix flake for a Haskell development environment.";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachDefaultSystem (system:

    let
      overlays = [
        haskellNix.overlay (final: prev: {
          haskell-snippets = final.haskell-nix.project' {
            src = ./.;
            compiler-nix-name = "ghc966";

            shell.tools = {
              cabal = {};
              hlint = {};
              stack = "3.1.1";
              haskell-language-server = {};
            };

            shell.buildInputs = with pkgs; [
              nixpkgs-fmt
            ];
          };
        })
      ];

      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

      flake = pkgs.haskell-snippets.flake { };

    in
      flake // { packages.default = flake.packages."snippets:exe:snippets"; });
}      

#{
  #description = "A Nix flake for Haskell development environment";

  #inputs = {
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  #};

  #outputs = { self, nixpkgs, ... }: let
    
    #system = "x86_64-linux";

  #in {
    #devShells."${system}".default = let
      #pkgs = import nixpkgs {
        #inherit system;
      #};

    #in pkgs.mkShell {
      #packages = with pkgs; [
        #cabal-install
        #stack
        #haskell.compiler.ghc8107
        #haskell-language-server
      #];

      #shellHook = ''
        #echo "ghc `${pkgs.haskell.compiler.ghc8107}/bin/ghc --version`"
      #'';
    #};
  #};
#}
