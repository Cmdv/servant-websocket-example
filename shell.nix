{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884", withHoogle ? false }:
let
  inherit (nixpkgs) pkgs;
  pinnedUnstable =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "90a7b7672cc2264b23cde7dc5273a532f1fb487b";
    };
  unstable = import pinnedUnstable {};
  ghcVersion = unstable.haskell.packages.${compiler};
  hspkgs =
    if withHoogle
       then
         ghcVersion.override {
           overrides = (self: super: {
             ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
             ghcWithPackages = self.ghc.withPackages;
           });
         }
       else ghcVersion;
  origBuild = hspkgs.callPackage ./fakie.nix {};
  drv = unstable.haskell.lib.overrideCabal origBuild (drv: {
    libraryToolDepends = [
      unstable.cabal-install
      unstable.hlint
      unstable.haskellPackages.haskell-language-server
      unstable.stdenv
      unstable.pkg-config
    ];
    librarySystemDepends = [ unstable.zlib ];
    license = unstable.stdenv.lib.licenses.bsd3;
    shellHook = ''
      '';
  });
in
  if pkgs.lib.inNixShell then drv.env else drv
