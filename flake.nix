{
  description = "wordle";

  inputs.nixpkgs.url = "nixpkgs/nixos-21.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        overlay = (final: prev: {
          wordle = (final.callPackage ./. { });
        });

      in rec {
        apps = { dev = pkgs.wordle.dev; };
        defaultApp = apps.dev;
        packages = {
          server = pkgs.wordle;
        };
        devShell = pkgs.wordle;
      });
}
