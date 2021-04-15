let
  pre-commit = import ./nix/pre-commit.nix;
  pkgs = import ./nix/pkgs.nix;

in
{
  "pre-commit-check" = pre-commit.check;
  "fld-logger" = pkgs.haskellPackages.fld-logger;
}
