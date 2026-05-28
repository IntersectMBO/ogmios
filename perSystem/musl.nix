{
  perSystem = {
    project,
    system,
    lib,
    ...
  }: let
    muslProject = project.projectCross.${
      if system == "x86_64-linux"
      then "musl64"
      else "aarch64-multiplatform-musl"
    };
    muslExes = muslProject.hsPkgs.ogmios.components.exes;
  in {
    packages.ogmios-musl = muslExes.ogmios;
  };
}
