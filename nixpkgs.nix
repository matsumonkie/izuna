let
  rev = "966489a0312f80e4dd20189f885bc12d6723a9ac";
  extractedTarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "sha256:10jx7y8bgkqki9nfy6fdg121pixysrrbdlyyy30sv4x65clmalwp";
  };
in
# extractedTarball will be a directory here, and 'import' will automatically append /default.nix here
import extractedTarball
