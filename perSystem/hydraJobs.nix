{ config, lib, withSystem, ... }: {
  flake.hydraJobs = lib.genAttrs config.systems (lib.flip withSystem (
    {
      config,
      pkgs,
      system,
      ...
    }: let
      integrationChecks = [ "ogmios-integration" ];
      ciChecks =
        if lib.hasSuffix "-linux" system
        then config.checks
        else removeAttrs config.checks integrationChecks;
      required = {
        inherit (config) packages;
        checks = removeAttrs ciChecks integrationChecks;
      };
      nonRequired = {
        inherit (config) devShells;
        checks = lib.filterAttrs (name: _: lib.elem name integrationChecks) ciChecks;
      };
      jobs = {
        inherit (config) packages devShells;
        checks = ciChecks;
      };
    in
      jobs
      // {
        required = pkgs.releaseTools.aggregate {
          name = "required";
          constituents = lib.collect lib.isDerivation required;
        };
        nonrequired = pkgs.releaseTools.aggregate {
          name = "nonrequired";
          constituents = lib.collect lib.isDerivation nonRequired;
        };
      }
  ));
}
