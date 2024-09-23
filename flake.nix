{
  description = "A Nix-flake-based Elm development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = (with pkgs.elmPackages; [
            elm
            elm-format
            elm-json
          ]) ++ (with pkgs; [
            elm2nix
            nodejs_18
          ]);
          shellHook = ''
            echo "Entered the nix shell..."
            export PATH=./node_modules/.bin:$PATH
          '';
        };
      });
    };
}
