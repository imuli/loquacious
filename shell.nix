{ pkgs ? import <nixpkgs> {}, hpkgs ? pkgs.haskellPackages }:
let mkDerivation = expr: hpkgs.mkDerivation (expr // {
      enableLibraryProfiling = false;
      enableSeparateDocOutput = true;
      doHaddock = true;
    });
in hpkgs.shellFor {
  packages = p: [ (import ./default.nix { inherit mkDerivation; }) ];
  withHoogle = true;
}
