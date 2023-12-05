{
  description = "A very basic flake";

  inputs = {
    uiua.url = "github:uiua-lang/uiua";
  };

  outputs = {
    self,
    nixpkgs,
    uiua,
    ...
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
  in {
    devShells.${system}.default = pkgs.mkShell {
      packages = [uiua.packages.${system}.default];
    };
  };
}
