let
  sources = import ./sources.nix;
  pkgsf = import sources.nixpkgs;
in
pkgsf {
  overlays = [
    (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
    (import ./overlay.nix)
  ];
}
