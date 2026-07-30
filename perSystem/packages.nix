{ inputs, ... }: {
  perSystem = { hsPkgs, pkgs, lib, system, ... }:
    let
      ogmios = hsPkgs.ogmios;
      integrationTestsExe =
        hsPkgs.ogmios-integration-tests.components.exes.ogmios-integration-tests;
      cn = inputs.cardano-node.packages.${system};
      cnTxGenerator = inputs.cardano-node-tx-generator.packages.${system};
    in
    {
      packages = {
        default = ogmios.components.exes.ogmios;
        ogmios = ogmios.components.exes.ogmios;
        ogmios-lib = ogmios.components.library;

        # The suite spawns ogmios, cardano-node, cardano-cli, cardano-testnet
        # and tx-generator (Test.Integration.Env resolves them from PATH), so
        # ship them as an execution dependency of the packaged binary:
        # `nix run .#ogmios-integration-tests` is self-contained. Inside the
        # `integration` devshell the same tools come from the shell's PATH
        # instead, and `cabal run` builds are intentionally left unwrapped so
        # they can be pointed at other tool versions.
        ogmios-integration-tests =
          pkgs.runCommand "ogmios-integration-tests"
            {
              nativeBuildInputs = [ pkgs.makeWrapper ];
              meta.mainProgram = "ogmios-integration-tests";
            }
            ''
              makeWrapper \
                ${integrationTestsExe}/bin/ogmios-integration-tests \
                $out/bin/ogmios-integration-tests \
                --prefix PATH : ${lib.makeBinPath [
                  ogmios.components.exes.ogmios
                  cn.cardano-node
                  cn.cardano-cli
                  cn.cardano-testnet
                  cnTxGenerator.tx-generator
                ]}
            '';
      };

      checks.ogmios-unit = ogmios.checks.unit;
    };
}
