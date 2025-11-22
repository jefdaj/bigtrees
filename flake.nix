{

  # This is an unusual flake because the final nix build is static, but the
  # dev shell is dynamic (regular). So often one can break while the other is still
  # fine.

  # Currently, the dynamic shell + stack build works and I'm adjusting the hs overrides for the static build
  # TODO cleanly separate the two visually below

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-25.05;
    flake-utils.url = "github:numtide/flake-utils";
    # TODO consider removing the git submodule in favor of this
    directory-tree = {
      url = github:jefdaj/directory-tree/bigtrees-revsort;
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, nixpkgs, flake-utils, directory-tree }:
    flake-utils.lib.eachDefaultSystem (system:
      let

        haskellOverlay = (final: prev: {

          # Currently this is ghc984, but ok to follow the default when it
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

              # Current overrides are pretty simple and based on stack.yaml
              Cabal               = hFinal.Cabal_3_14_2_0;
              Cabal-syntax        = hFinal.Cabal-syntax_3_14_2_0;
              directory           = hFinal.callHackage "directory" "1.3.8.5" {};
              unix                = hFinal.callHackage "unix" "2.8.6.0" {};
              file-io             = hFinal.callHackage "file-io" "0.1.5" {};
              process             = hFinal.callHackage "process" "1.6.25.0" {};
              filepath            = hFinal.callHackage "filepath" "1.5.4.0" {};
              filepath-bytestring = hFinal.callHackage "filepath-bytestring" "1.5.2.0.2" {};

              # Some old examples of more complicated overrides for reference:
              # Cabal        = hFinal.Cabal_3_10_3_0;
              # Cabal-syntax = hFinal.Cabal-syntax_3_10_3_0;
              # docopt   = hFinal.callHackage "docopt" "0.7.0.8" {};
              # process  = hFinal.callHackage "process" "1.6.20.0" {};
              # directory = final.haskell.lib.doJailbreak (hFinal.callHackage "directory" "1.3.8.2" {});
              # file-io   = final.haskell.lib.doJailbreak (hFinal.callHackage "file-io" "0.1.1" {});
              # MissingH = final.haskell.lib.doJailbreak hPrev.MissingH;
              # hashable = final.haskell.lib.doJailbreak (hFinal.callHackageDirect {
              #   pkg = "hashable";
              #   ver = "1.4.6.0";
              #   sha256 = "sha256-UK24kyPDWNwkmSJP04DATlXRrfmX+mWBUeGaO4ZYgTM=";
              # } {});
              # os-string = hFinal.callHackageDirect {
              #   pkg = "os-string";
              #   ver = "2.0.3";
              #   sha256 = "sha256-dX6TlnZnZswoolVBGhOAifuVRgCApojto3CzhCaYITs=";
              # } {};

            };
          };
        });

        # attempt at cross compilation for macos...
        pkgsDynamic = pkgsDynamicOptions.${system};
        mkPkgsDynamic = targetArch:
          let attrs = { system = "x86_64-linux"; overlays = [ haskellOverlay ]; };
          in import nixpkgs (
            if targetArch == "x86_64-linux"
              then attrs
              else (attrs // { crossSystem = { config = targetArch; }; })
          );
        pkgsDynamicOptions =
          builtins.listToAttrs
            (map (t: { name = t; value = mkPkgsDynamic t; })
            flake-utils.lib.defaultSystems);

        # these both work (on x86_64-linux only) and seem equivalent:
        # pkgsDynamic = nixpkgs.legacyPackages.${system}.extend haskellOverlay;
        # pkgsDynamic = (import nixpkgs {
        #   inherit system;
        #   overlays = [ haskellOverlay ];
        # });

        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix: We don't want Stack's way of integrating Nix.
        # --system-ghc      # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgsDynamic.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgsDynamic.stack ];

          # TODO does this help anything?
          # nativeBuildInputs = [
          #   pkgsDynamic.pkg-config
          # ];

          buildInputs = [ pkgsDynamic.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

        hPkgs =
          pkgsDynamic.haskell.packages."ghc984"; # need to match Stackage LTS version
                                                 # from stack.yaml snapshot

        myDevTools = [

          # basics
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          pkgsDynamic.zlib # External C library needed by some Haskell packages
          stack-wrapped

          # dev
          hPkgs.ghcid # Continuous terminal Haskell compile checker
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.hoogle # Lookup Haskell documentation
          # pkgs.vscode

          # TODO any of these helpful? They're from the example so should work
          # hPkgs.ormolu # Haskell formatter
          # hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          # hPkgs.retrie # Haskell refactoring tool
          # hPkgs.cabal-install

          # analyze/lint
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.apply-refact
          pkgsDynamic.stylish-haskell
          hPkgs.weeder
          hPkgs.stan

          # tests/examples
          pkgsDynamic.tree
          pkgsDynamic.bats
          pkgsDynamic.rsync
          pkgsDynamic.curl

        ];

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

        # The dev tools could probably also be static, but why rebuild them?
        # devShells.default = pkgs.mkShell {
        devShell = pkgsDynamic.mkShell {
          nativeBuildInputs = [ pkgsDynamic.pkg-config ];
          buildInputs = myDevTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgsDynamic.lib.makeLibraryPath myDevTools;

          # TODO is this still needed/helpful?
          # Configure the Nix path to our own `pkgs`, to ensure Stack-with-Nix uses the
          # correct one rather than the global <nixpkgs> when looking for the right
          # `ghc` argument to pass in `nix/stack-integration.nix`
          # See https://nixos.org/nixos/nix-pills/nix-search-paths.html for more information
          # NIX_PATH = "nixpkgs=" + pkgs.path;

          # This seems to be necessary when running tests that capture stdout/stderr
          # TODO still?
          TASTY_NUM_THREADS = 1;
        };

      });
}
