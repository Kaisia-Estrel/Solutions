{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: 
    let 
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
  {

    devShells.${system}.default = pkgs.mkShell {
      packages = with pkgs; [
        nodePackages.prettier
        nodePackages.vscode-html-languageserver-bin
        nodePackages.vscode-css-languageserver-bin
        nodePackages.eslint
        nodePackages.eslint_d
        nodePackages.typescript-language-server
        nodePackages.typescript
      ];
    };

  };
}
