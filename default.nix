{ compiler ? "ghc8104"
, rev      ? "7e9b0dff974c89e070da1ad85713ff3c20b0ca97"
, sha256   ? "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36"
, pkgs     ?
    import (builtins.fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256; }) {
      config.allowBroken = false;
      config.allowUnfree = true;
    }
}:
let
  nix-thunk-src = (pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "nix-thunk";
    rev = "bab7329163fce579eaa9cfba67a4851ab806b76f";
    sha256 = "0wn96xn6prjzcsh4n8p1n40wi8la53ym5h2frlqbfzas7isxwygg";
  });
  inherit (import nix-thunk-src {}) thunkSource;
  gitignoreSrc = import (pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "2ced4519f865341adcb143c5d668f955a2cb997f";
    sha256 = "0fc5bgv9syfcblp23y05kkfnpgh3gssz6vn24frs8dzw39algk2z";
  }) {};
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
in pkgs.haskell.packages.${compiler}.developPackage {
  name = "keychain";
  root = gitignoreSrc.gitignoreSource ./.;

  overrides = self: super: {
    cardano-crypto = self.callCabal2nix "cardano-crypto" (thunkSource ./dep/cardano-crypto) {};
    some = self.callHackageDirect {
      pkg = "some";
      ver = "1.0.3";
      sha256 = "1mrraqn7pz635rjrff5ih6j9srmivm53bn8jd92qpi8ikbvlva19";
    } {};
#    dependent-sum-template = self.callHackageDirect {
#      pkg = "dependent-sum-template";
#      ver = "0.1.0.0";
#      sha256 = "0fm73cbja570lfxznv66daya5anp4b0m24jjm5fwn95f49dp9d4n";
#      # sha256 = pkgs.lib.fakeSha256;
#    } {};
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      pkgs.haskell.packages.${compiler}.cabal-install
      pkgs.haskell.packages.${compiler}.ghcid
    ];
  });
}
