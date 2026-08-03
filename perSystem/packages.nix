{ inputs, ... }: {
  perSystem = { hsPkgs, pkgs, lib, system, ... }:
    let
      ogmios = hsPkgs.ogmios;
      integrationTestsExe =
        hsPkgs.ogmios-integration-tests.components.exes.ogmios-integration-tests;
      cn = inputs.cardano-node.packages.${system};
      cnTxGenerator = inputs.cardano-node-tx-generator.packages.${system};

      # The suite spawns ogmios, cardano-node, cardano-cli, cardano-testnet
      # and tx-generator (Test.Integration.Env resolves them from PATH), so
      # ship them as an execution dependency of the packaged binary:
      # `nix run .#ogmios-integration-tests` is self-contained. Inside the
      # `integration` devshell the same tools come from the shell's PATH
      # instead, and `cabal run` builds are intentionally left unwrapped so
      # they can be pointed at other tool versions.
      integrationTests =
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
    in
    {
      packages = {
        default = ogmios.components.exes.ogmios;
        ogmios = ogmios.components.exes.ogmios;
        ogmios-lib = ogmios.components.library;
        ogmios-integration-tests = integrationTests;
      };

      checks = {
        ogmios-unit = ogmios.checks.unit;

        # Actually executes the integration suite: a throwaway testnet,
        # ogmios and tx-generator all run inside the build, talking over
        # loopback. Defined for every system so it can always be run by
        # hand (`nix build .#checks.<system>.ogmios-integration`, or
        # `nix flake check`); hydra however only schedules it on Linux,
        # where the sandbox's network namespace keeps the suite's fixed
        # ogmios port private to the build - see hydraJobs.nix.
        ogmios-integration =
          pkgs.runCommand "ogmios-integration"
            {
              nativeBuildInputs = [ integrationTests ];
              # Sandboxed darwin builds block networking by default; the
              # suite only ever talks to 127.0.0.1.
              __darwinAllowLocalNetworking = true;
              meta = {
                description = "run the ogmios integration suite against a local testnet";
                # ~2 minutes when healthy; don't let a wedged daemon hold a
                # builder for hours.
                timeout = 1800;
              };
            }
            ''
              ogmios-integration-tests || {
                echo "==== integration suite failed; daemon log tails follow"
                for f in "$TMPDIR"/ogmios-integration-*/logs/*; do
                  echo "==== $f"
                  tail -n 40 "$f" || true
                done
                exit 1
              }
              touch $out
            '';
      };
    };
}
