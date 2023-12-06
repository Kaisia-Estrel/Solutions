{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nuget-packageslock2nix = {
      url = "github:mdarocha/nuget-packageslock2nix/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    nixpkgs,
    nuget-packageslock2nix,
    ...
  }: {
    packages.x86_64-linux.default = let
      pkgs = import nixpkgs {system = "x86_64-linux";};
    in
      pkgs.buildDotnetModule {
        pname = "e-track";
        version = "0.0.1";
        src = ./.;

        dotnet-sdk = pkgs.dotnetCorePackages.sdk_8_0;
        dotnet-runtime = pkgs.dotnetCorePackages.runtime_8_0;

        nugetDeps = nuget-packageslock2nix.lib {
          system = "x86_64-linux";
          name = "e-track";
          lockfiles = [
            ./ETrack.Api/packages.lock.json
            ./ETrack.Web/packages.lock.json
            ./ETrack.Models/packages.lock.json
          ];
        };
      };

    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;

    devShells.x86_64-linux.default = let
      pkgs = import nixpkgs {system = "x86_64-linux";};
    in
      pkgs.mkShell {
        buildInputs = with pkgs; [
          omnisharp-roslyn
          dotnetCorePackages.sdk_8_0
          dotnetCorePackages.runtime_8_0
          sqlite
          sqlite-web
        ];
      };
  };
}
