{
  perSystem = {
    project,
    system,
    lib,
    ...
  }: let
    # Only offer the static build from a Linux host. Cross-compiling to
    # linux-musl from darwin means asking haskell.nix for a whole
    # aarch64-unknown-linux-musl GHC that no cache has a substitute for, so it
    # gets built from source and times out on the CI builders.
    hostIsLinux = lib.hasSuffix "-linux" system;
    muslProject = project.projectCross.${
      if system == "x86_64-linux"
      then "musl64"
      else "aarch64-multiplatform-musl"
    };
    muslExes = muslProject.hsPkgs.ogmios.components.exes;
  in
    lib.optionalAttrs hostIsLinux {
      packages.ogmios-musl = muslExes.ogmios;
    };
}
