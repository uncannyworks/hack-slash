let
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          hack-slash =
            self.callPackage ./packages.nix { };
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  {
    hack-slash = pkgs.haskellPackages.hack-slash;
  }