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

  overrides = self: super: with pkgs.haskell.lib; {
    cardano-crypto = self.callCabal2nix "cardano-crypto" (thunkSource ./dep/cardano-crypto) {};
    base16 = dontCheck (self.callHackageDirect {
      pkg = "base16";
      ver = "0.3.0.1";
      sha256 = "1fiyp23zlac87bqrirq1b40v51b76icd9razzicx6ixldglqjqlj";
    } {});
    base64 = self.callHackageDirect {
      pkg = "base64";
      ver = "0.4.2.3";
      sha256 = "1i4cf1xfbkxlxshwlsxgw2w5gi3hkkfm1n99vnzq7rixz8nxcw7r";
    } {};
    some = self.callHackageDirect {
      pkg = "some";
      ver = "1.0.3";
      sha256 = "1mrraqn7pz635rjrff5ih6j9srmivm53bn8jd92qpi8ikbvlva19";
    } {};
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      pkgs.haskell.packages.${compiler}.cabal-install
      pkgs.haskell.packages.${compiler}.ghcid
    ];
  });
}
