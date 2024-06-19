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

              # Most of these default to the versions in stackage.org/lts-21.25,
              # the latest LTS for ghc948. It's usually easier to find out a set of working
              # packages by iterating in stack.yaml, then copy the versions here.
              #
              # Broken: callHackageDirect
              # Broken: doJailbreak on existing packages in the main set
              # Working: doJailbreak + callHackage
              #
              # Glob      = hPrev.callHackage "Glob"      "0.10.2"  {};
              # directory = hPrev.callHackage "directory" "1.3.7.1" {};
              docopt    = hPrev.callHackage "docopt"    "0.7.0.8" {};
              unix      = hPrev.callHackage "unix"      "2.8.5.1" {};
              filepath  = final.haskell.lib.doJailbreak (hPrev.callHackage "filepath" "1.5.2.0"  {});
              cmdargs   = final.haskell.lib.doJailbreak (hPrev.callHackage "cmdargs"  "0.10.22"  {});
              process   = final.haskell.lib.doJailbreak (hPrev.callHackage "process"  "1.6.18.0" {});

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
