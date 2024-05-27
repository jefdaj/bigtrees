{

  inputs = {

    # domenkozar+angerman fix for ghc static build failing is to use haskell.nix for now:
    haskellNix.url = "github:input-output-hk/haskell.nix?ref=angerman/fix-aarch64-musl";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
    # TODO consider removing the git submodule in favor of this
    directory-tree = {
      url = github:jefdaj/directory-tree/isname-typeclass;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, haskellNix, flake-utils, directory-tree }:
    flake-utils.lib.eachDefaultSystem (system:

      # TODO what was legacyPackages for?
      # with nixpkgs.legacyPackages.${system}.pkgsStatic;

      let

        # Ah, another nixpkgs bug/maybe ghc this time?
        # https://github.com/NixOS/nixpkgs/issues/275304
        #
        # @domenkozar reproduced the problem, and fixed by using haskellNix:
        # https://github.com/domenkozar/nixpkgs-static-repo/tree/haskell.nix
        #
        # versions tried:
        #   nixos-unstable:
        #     ghc982 (latest ghc on nixos-unstable) th-orphans fails
        #   nixos-23.11:
        #     ghc981
        #   nixpkgs-unstable:
        #     ghc9101 (latest ghc on anything ppl use?) cabal-doctest fails
        #     ghc982 (second latest on unstable) th-orphans fails
        #     ghc981 th-orphans fails
        #     ghc963-4 ghc itself marked broken
        #     ghc948 ...
        #
        myGhcVersion = "ghc962";

        # https://cs-syd.eu/posts/2024-04-20-static-linking-haskell-nix
        # Unnecessary these days? The same exact ghc builds with and without it.
        fixGHC = pkg: pkg.override {    
          enableRelocatedStaticLibs = true;
          enableShared = false;
        };

        # This overlay is weird because it needs to work around a nixpkgs haskell bug:
        # https://github.com/NixOS/nixpkgs/issues/235960
        # The fix is to expose all new attributes "my*" for now.
        # This post pointed me in the right direction:
        # https://discourse.nixos.org/t/working-with-haskell-broken-packages/30126/5
        # TODO would just making the outermost myHaskell different be enough?
        # TODO did i successfully work around the fix-point issue, or did ghc981 fix docopt?
        haskellOverlay = (final: prev: {
          myHaskell = final.lib.recursiveUpdate prev.haskell {
            myPackages = final.lib.recursiveUpdate prev.haskell.packages {
              myGhc = prev.haskell.packages.${myGhcVersion}.override {
                overrides = hFinal: hPrev: {

                  ghc = fixGHC hPrev.ghc;

                  # TODO figure out how to include the DT flake output directly instead?
                  directory-tree = hFinal.callCabal2nix "directory-tree" directory-tree {};
                  docopt = prev.haskell.lib.markUnbroken hPrev.docopt;
                };
              };
            };
          };
        });

        # this style also works:
        # pkgsDynamic = nixpkgs.legacyPackages.${system}.extend haskellOverlay;
        pkgsDynamic = (import nixpkgs {
          inherit system;
          overlays = [ haskellOverlay ];
        });

      # Static by default, but allow pkgsDynamic to be referenced explicitly
      # for dev tools we don't want to bother rebuilding.
      in with pkgsDynamic.pkgsStatic;
      let

        project = devTools:
        let
          addBuildTools = lib.trivial.flip myHaskell.lib.addBuildTools devTools;
          confirmStaticBinaries = lib.trivial.flip myHaskell.lib.overrideCabal (old: {
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

        # TODO is there a nicer way to load it in nix repl?
        # inherit myHaskell;

        # empty devTools tells it to build the package
        packages.pkg = project [ ];

        defaultPackage = self.packages.${system}.pkg;

        executableSystemDepends = [
          # gitAndTools.git
          # gitAndTools.gitAnnex
          # rsync
        ];

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
