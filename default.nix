{ nixpkgs ? import <nixpkgs> {} }:
let
  ghc = nixpkgs.haskellPackages.extend(self: super: {
    persistent-query = self.callCabal2nix "persistent-query" ./. {};
  });
in
  ghc.persistent-query
