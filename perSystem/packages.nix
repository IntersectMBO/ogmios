{
  perSystem = { hsPkgs, ... }:
    let
      ogmios = hsPkgs.ogmios;
    in
    {
      packages = {
        default = ogmios.components.exes.ogmios;
        ogmios = ogmios.components.exes.ogmios;
        ogmios-lib = ogmios.components.library;
        ogmios-integration-tests = hsPkgs.ogmios-integration-tests.components.exes.ogmios-integration-tests;
      };

      checks.ogmios-unit = ogmios.checks.unit;
    };
}
