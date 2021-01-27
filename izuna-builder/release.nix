let
  rev = "966489a0312f80e4dd20189f885bc12d6723a9ac";
  extractedTarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "sha256:10jx7y8bgkqki9nfy6fdg121pixysrrbdlyyy30sv4x65clmalwp";
  };

  mkConfig = compiler: {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              servant-multipart =
                haskellPackagesNew.callPackage ./servant-multipart.nix { };

              project =
                haskellPackagesNew.callPackage ./project.nix { };
            };
          };
        };
      };
    };
  };

  mkPkgs = compiler: import extractedTarball { config = ( mkConfig compiler ); };

in
{ izuna-builder-8101 = (mkPkgs "ghc8101").haskellPackages.callPackage ./project.nix { };
  izuna-builder-8102 = (mkPkgs "ghc8102").haskellPackages.callPackage ./project.nix { };
  izuna-builder-8103 = (mkPkgs "ghc8103").haskellPackages.callPackage ./project.nix { };
}
