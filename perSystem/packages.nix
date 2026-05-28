{
  perSystem = { hsPkgs, ... }:
    let
      ogmios = hsPkgs.ogmios;
    in
    {
      packages = {
        default = ogmios.components.exes.ogmios;
        ogmios = ogmios.components.library;
        ogmios-exe = ogmios.components.exes.ogmios;
      };

      checks.ogmios-unit = ogmios.checks.unit;
    };
}
