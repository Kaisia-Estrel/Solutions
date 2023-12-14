{
  description = "A very basic flake";

  outputs = {
    nixpkgs,
    ...
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
  in {
    devShells.${system}.default = pkgs.mkShell {
      packages = [
        (pkgs.python311.withPackages (ps: with ps; [python-lsp-server black]))
        pkgs.poetry
      ];
    };
  };
}
