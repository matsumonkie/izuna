let
  rev = "966489a0312f80e4dd20189f885bc12d6723a9ac";
  extractedTarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "sha256:10jx7y8bgkqki9nfy6fdg121pixysrrbdlyyy30sv4x65clmalwp";
  };

  mkConfig = compiler: {
    allowBroken = true;
    doCheck = false;
    doHaddock = false;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              servant-multipart =
                haskellPackagesNew.callPackage ./servant-multipart.nix { };

              izuna-builder =
                haskellPackagesNew.callPackage ./project.nix { ghcVersion = "${compiler}"; };
            };
          };
        };
      };
    };
  };

  mkPkgs = compiler: import extractedTarball { config = ( mkConfig compiler ); };

  mkIzunaBuilder = ghcVersion: (mkPkgs ghcVersion).haskell.packages."${ghcVersion}".izuna-builder;
in
{ izuna-builder-8101 = (mkPkgs "ghc8101").haskell.packages.ghc8101.izuna-builder;
  izuna-builder-8102 = mkIzunaBuilder "ghc8102";
  izuna-builder-8103 = mkIzunaBuilder "ghc8103";
  pkgs = import extractedTarball { }; # for dev
}
