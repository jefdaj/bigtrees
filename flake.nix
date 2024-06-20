{

  inputs = {
    # TODO stable release? nixpkgs-unstable? see what ppl are doing these days
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    flake-utils.url = "github:numtide/flake-utils";
    # TODO consider removing the git submodule in favor of this
    directory-tree = {
      url = github:jefdaj/directory-tree/wip-for-bigtrees;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, directory-tree }:
    flake-utils.lib.eachDefaultSystem (system:

      # TODO what was legacyPackages for?
      # with nixpkgs.legacyPackages.${system}.pkgsStatic;

      let

        # TODO why is docopt still blocking evaluation after markUnbroken?
        #      oh, there's a bug:
        #      https://github.com/NixOS/nixpkgs/issues/235960
        #      exposing it as haskellPackages does not help
        haskellOverlay = (final: prev: {

          # Currently this is ghc948, but ok to follow the default when it
          # updates. Just remember to update the package versions below too, by
          # removing them and adding new overrides as needed until it builds.
          # myHaskellPackages = prev.haskell.packages.ghc948.override {
          myHaskellPackages = prev.haskellPackages.override {
            overrides = hFinal: hPrev: {

              # TODO figure out how to include the DT flake output directly instead?
              directory-tree = hFinal.callCabal2nix "directory-tree" directory-tree {};

              # It's usually easier to find a set of working packages by
              # iterating in stack.yaml, then translate the working versions
              # into nix overrides here.

              Cabal        = hFinal.Cabal_3_10_3_0;
              Cabal-syntax = hFinal.Cabal-syntax_3_10_3_0;

              docopt   = hFinal.callHackage "docopt" "0.7.0.8" {};
              filepath = hFinal.callHackage "filepath" "1.5.2.0" {};
              process  = hFinal.callHackage "process" "1.6.20.0" {};

              directory = final.haskell.lib.doJailbreak (hFinal.callHackage "directory" "1.3.8.2" {});
              file-io   = final.haskell.lib.doJailbreak (hFinal.callHackage "file-io" "0.1.1" {});
              unix      = final.haskell.lib.doJailbreak (hFinal.callHackage "unix" "2.8.5.1" {});

              MissingH = final.haskell.lib.doJailbreak hPrev.MissingH;

              hashable = final.haskell.lib.doJailbreak (hFinal.callHackageDirect {
                pkg = "hashable";
                ver = "1.4.6.0";
                sha256 = "sha256-UK24kyPDWNwkmSJP04DATlXRrfmX+mWBUeGaO4ZYgTM=";
              } {});

              os-string = hFinal.callHackageDirect {
                pkg = "os-string";
                ver = "2.0.3";
                sha256 = "sha256-dX6TlnZnZswoolVBGhOAifuVRgCApojto3CzhCaYITs=";
              } {};

            };
          };
        });

        # also works, except also has the fix-point bug:
        # pkgsDynamic = nixpkgs.legacyPackages.${system}.extend haskellOverlay;
        pkgsDynamic = (import nixpkgs {
          inherit system;
          overlays = [ haskellOverlay ];
        });

      # Static by default, but allow pkgsDynamic to be referenced explicitly for dev tools.
      in with pkgsDynamic.pkgsStatic;
      let

        project = devTools:
        let
          addBuildTools = lib.trivial.flip haskell.lib.addBuildTools devTools;
          confirmStaticBinaries = lib.trivial.flip haskell.lib.overrideCabal (old: {
            # https://cs-syd.eu/posts/2024-04-20-static-linking-haskell-nix
            # We want to confirm it says something like this:
            # ldd: /nix/store/.../bin/bigtrees: Not a valid dynamic program
            postInstall = (old.postInstall or "") + '' for b in $out/bin/*; do
                if ldd "$b"; then
                  echo "ldd succeeded on $b, which may mean that it is not statically linked"
                  exit 1
                fi
              done
            '';
          });

        in myHaskellPackages.developPackage {
          # root = lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ".txt" ];
          root = lib.cleanSource ./.;
          name = "bigtrees";
          returnShellEnv = !(devTools == [ ]);
          modifier = (lib.trivial.flip lib.trivial.pipe) [
            addBuildTools
            haskell.lib.dontHaddock
            haskell.lib.enableStaticLibraries
            haskell.lib.justStaticExecutables
            haskell.lib.disableLibraryProfiling
            haskell.lib.disableExecutableProfiling
            confirmStaticBinaries
          ];
        };

      in rec {
        # empty devTools tells it to build the package
        packages.pkg = project [ ];

        defaultPackage = self.packages.${system}.pkg;

        executableSystemDepends = [
          # gitAndTools.git
          # gitAndTools.gitAnnex
          # rsync
        ];

        # The dev tools could probably also be static, but why rebuild them?
        devShell = project (executableSystemDepends ++ [

          # TODO *any* package here evaluates the broken docopt? weird
          # hello

          # cabal-fmt
          # cabal-install TODO why does this fail on perl?
          # haskell-language-server
          # hlint

          # test
          # pkgsDynamic.tree

          # analyze/lint
          # pkgsDynamic.hlint
          # pkgsDynamic.haskellPackages.apply-refact
          # stylish-haskell
          # pkgsDynamic.haskellPackages.weeder
          # pkgsDynamic.haskellPackages.stan

        ]);
      });
}
