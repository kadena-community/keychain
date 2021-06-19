{ kpkgs ? import ./dep/kpkgs {} }:
let
  pkgs = kpkgs.pkgs;
  nix-thunk-src = (pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "nix-thunk";
    rev = "bab7329163fce579eaa9cfba67a4851ab806b76f";
    sha256 = "0wn96xn6prjzcsh4n8p1n40wi8la53ym5h2frlqbfzas7isxwygg";
  });
  inherit (import nix-thunk-src {}) thunkSource;
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
    sha256 = "0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

  # Haskell package set, useful for overriding source-based deps
  haskellOverrides = self: super: {
    cardano-crypto = self.callCabal2nix "cardano-crypto" (thunkSource ./dep/cardano-crypto) {};
    some = self.callHackageDirect {
      pkg = "some";
      ver = "1.0.3";
      sha256 = "1mrraqn7pz635rjrff5ih6j9srmivm53bn8jd92qpi8ikbvlva19";
      # sha256 = pkgs.lib.fakeSha256;
    } {};
  };
  src = gitignoreSource ./.;

in pkgs.haskellPackages.developPackage {
  root = src;
  name = "keychain";
  overrides = haskellOverrides;
  # source-overrides ? {}
  # returnShellEnv ? pkgs.lib.inNixShell
  # modifier ? drv: drv
  # withHoogle ? returnShellEnv
  # cabal2nixOptions ? ""
}
