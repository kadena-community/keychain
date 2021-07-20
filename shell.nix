(import ./. {}).overrideAttrs (oldDrv: {
  nativeBuildInputs = with (import ./dep/kpkgs {}).pkgs;
    [ cabal-install ghcid ]
    ++ oldDrv.nativeBuildInputs;
  })
