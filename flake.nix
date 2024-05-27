{

  inputs = {
    # TODO stable release? nixpkgs-unstable? see what ppl are doing these days
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    flake-utils.url = "github:numtide/flake-utils";
    # TODO consider removing the git submodule in favor of this
    directory-tree = {
      url = github:jefdaj/directory-tree/isname-typeclass;
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
          myHaskell = final.lib.recursiveUpdate prev.haskell {
            myPackages = final.lib.recursiveUpdate prev.haskell.packages {
              myGhc = prev.haskell.packages.ghc981.override {
          # myHaskellPackages = prev.haskellPackages.override {
                overrides = hFinal: hPrev: {
                  # TODO figure out how to include the DT flake output directly instead?
                  directory-tree = hFinal.callCabal2nix "directory-tree" directory-tree {};
                  docopt = final.haskell.lib.markUnbroken hPrev.docopt;
                };
              };
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
          addBuildTools = lib.trivial.flip myHaskell.lib.addBuildTools devTools;
          confirmStaticBinaries = lib.trivial.flip myHaskell.lib.overrideCabal (old: {
            # https://cs-syd.eu/posts/2024-04-20-static-linking-haskell-nix
            postInstall = (old.postInstall or "") + '' for b in $out/bin/*; do
                if ldd "$b"; then
                  echo "ldd succeeded on $b, which may mean that it is not statically linked"
                  exit 1
                fi
              done
            '';
          });

        in myHaskell.myPackages.myGhc.developPackage {
          # root = lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ".txt" ];
          root = lib.cleanSource ./.;
          name = "bigtrees";
          returnShellEnv = !(devTools == [ ]);
          modifier = (lib.trivial.flip lib.trivial.pipe) [
            addBuildTools
            myHaskell.lib.dontHaddock
            myHaskell.lib.enableStaticLibraries
            myHaskell.lib.justStaticExecutables
            myHaskell.lib.disableLibraryProfiling
            myHaskell.lib.disableExecutableProfiling
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
