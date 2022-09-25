{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, flake-utils, nixpkgs, easy-purescript-nix }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          easy-ps = import easy-purescript-nix { inherit pkgs; };
        in
        {
          devShell = import ./shell.nix { inherit pkgs; inherit easy-ps; };
        }
      );
}
