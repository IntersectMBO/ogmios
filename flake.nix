{
  inputs = {
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackageNix";
    };
    # Tools for the integration suite: cardano-node, cardano-cli,
    # cardano-testnet and tx-generator, all from the release the server is
    # built against. 11.1.1 carries tx-generator's Ogmios submission
    # transport (IntersectMBO/cardano-node#6609), so the previous separate
    # tx-generator pin is folded back in.
    cardano-node.url = "github:IntersectMBO/cardano-node/11.1.1";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    self.submodules = true;
  };

  outputs = inputs: let
    inherit ((import ./flake/lib.nix {inherit inputs;}).flake.lib) recursiveImports;
  in
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      imports = recursiveImports [./perSystem] ++ [inputs.flake-parts.flakeModules.touchup];
      touchup.attr.formatter.enable = false;
      systems = [
        "x86_64-linux"
        # "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem = {system, ...}: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          inherit (inputs.haskellNix) config;
          overlays = [
            inputs.iohkNix.overlays.crypto
            inputs.haskellNix.overlay
            inputs.iohkNix.overlays.haskell-nix-extra
            inputs.iohkNix.overlays.haskell-nix-crypto
            inputs.iohkNix.overlays.cardano-lib
            inputs.iohkNix.overlays.utils
          ];
        };
      };
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
