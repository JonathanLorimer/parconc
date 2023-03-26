{
  description = "parconc";

  inputs = {
    # Nix Inputs
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }: let
    utils = flake-utils.lib;
  in
    utils.eachDefaultSystem (system: let
      compilerVersion = "ghc925";
      pkgs = nixpkgs.legacyPackages.${system};
      hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
        overrides = hfinal: hprev: {
          parconc = hfinal.callCabal2nix "parconc" ./. {};
          # Needed for hls on ghc 9.2.5 and 9.4.3
          # https://github.com/ddssff/listlike/issues/23
          ListLike = pkgs.haskell.lib.dontCheck hprev.ListLike;
        };
      };
    in {
      # nix build
      packages =
        utils.flattenTree
        {
          parconc = hsPkgs.parconc;
          default = hsPkgs.parconc;
        };

      # You can't build the parconc package as a check because of IFD in cabal2nix
      checks = {};

      # nix fmt
      formatter = pkgs.alejandra;

      # nix develop
      devShell = hsPkgs.shellFor {
        withHoogle = true;
        packages = p: [
          p.parconc
        ];
        buildInputs = with pkgs;
          [
            hsPkgs.haskell-language-server
            haskellPackages.cabal-install
            cabal2nix
            haskellPackages.ghcid
            haskellPackages.fourmolu
            haskellPackages.cabal-fmt
            nodePackages.serve
          ]
          ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
      };

      # nix run
      apps = let
        parconc = utils.mkApp {
          drv = self.packages.${system}.default;
        };
      in {
        inherit parconc;
        default = parconc;
      };
    });
}
