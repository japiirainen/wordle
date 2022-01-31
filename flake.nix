{
  description = "Simple haskell nix flake";

  nixConfig.bash-prompt = "[nix]\\e\[38;5;172mλ \\e\[m";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.05";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { flake-utils, nixpkgs, self }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        overlay = (final: prev: {
          wordle = (final.callPackage ./. { });
        });
      in
      rec {
        inherit overlay;
        apps = { dev = pkgs.wordle; };
        defaultApp = apps.dev;
        packages = {
          wordle = pkgs.wordle;
        };
        defaultPackage = packages.wordle;
        checks = packages;
        devShell = pkgs.wordle;
        devShells = {
          wordle = pkgs.wordle;
        };
      });
}
