let
  rev = "2c0f6135aab77ff942b615228882c7dd996e0882";
  extractedTarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "sha256:00yg8dck4ymcifrxsknnpms52n16xnb8yiqjkby002mbm2aflf45";
  };
in
  # extractedTarball will be a directory here, and 'import' will automatically append /default.nix here
import extractedTarball
