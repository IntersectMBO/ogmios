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
      };

      checks.ogmios-unit = ogmios.checks.unit;
    };
}
