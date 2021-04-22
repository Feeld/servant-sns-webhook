let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix;
  pre-commit = import ./nix/pre-commit.nix;

in
pkgs.haskell.lib.buildStackProject {
  name = "fld-logger-shell";
  buildInputs = with pkgs; [
    (import sources.niv { inherit pkgs; }).niv
    pcre
    pkg-config
    zlib
    gnumake
  ] ++ pre-commit.tools;
  shellHook = ''
    export TMPDIR=/tmp
    ${pre-commit.check.shellHook}
  '';
}
