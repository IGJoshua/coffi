{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (final: prev: {
            clojure = prev.clojure.override { jdk = final.jdk22; };
          })
        ];
      };
    in
      {
        devShells.${system}.default = pkgs.mkShell rec {
          packages = [
          ];

          nativeBuildInputs = with pkgs; [
            clojure
          ];

          buildInputs = with pkgs; [
          ];

          inputsFrom = with pkgs; [
          ];
        };
      };
}
