(import ./. {}).overrideAttrs (oldDrv: {
  nativeBuildInputs = with (import ./dep/kpkgs {}).pkgs;
    [ cabal-install ghcid stylish-haskell stylish-cabal hlint haskellPackages.hindent ] 
    ++ oldDrv.nativeBuildInputs;
  })
