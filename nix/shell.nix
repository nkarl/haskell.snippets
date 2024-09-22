{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.shellFor {
  #packages = _: [ ( pkgs.haskellPackages.callCabal2nix "haskell/snippets" ./. {} ) ];
  packages = _: [ ];
  withHoogle = true;
  buildInputs = [
    pkgs.stack
    pkgs.cabal-install
    pkgs.haskell-language-server
  ];
}
