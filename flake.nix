{
  description = "YAMD — indentation-based extensible markup language (Racket)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      git-hooks,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Git hooks are generated from this config; `.pre-commit-config.yaml`
        # is written by the dev-shell hook (and gitignored), not checked in.
        # Golden expectation files are byte-exact fixtures and the tree-sitter
        # `src/` is machine-generated, so the whitespace hooks must leave both
        # untouched.
        generated = [
          "^tests/golden/"
          "^editors/tree-sitter-yamd/src/"
        ];
        pre-commit-check = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixfmt-rfc-style.enable = true;
            trim-trailing-whitespace = {
              enable = true;
              excludes = generated;
            };
            end-of-file-fixer = {
              enable = true;
              excludes = generated;
            };
            check-added-large-files.enable = true;
            check-json.enable = true;
            check-yaml.enable = true;
            check-merge-conflicts.enable = true;
          };
        };
      in
      {
        devShells.default = pkgs.mkShell {
          inherit (pre-commit-check) shellHook;
          packages = [
            pkgs.racket
            pkgs.git
          ]
          ++ pre-commit-check.enabledPackages;
        };

        # `nix run . -- file.yamd [--backend ...] [-o out]` renders a document.
        # The yamd collection is placed on PLTCOLLECTS and precompiled; the
        # wrapper trusts the prebuilt bytecode (PLT_COMPILED_FILE_CHECK=exists)
        # so runs do not try to recompile into the read-only store.
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "yamd";
          version = "0.1";
          src = self;
          nativeBuildInputs = [
            pkgs.racket
            pkgs.makeWrapper
          ];
          buildPhase = ''
            export HOME=$TMPDIR
            # Make the in-tree `yamd` collection resolvable so absolute
            # self-requires (e.g. `yamd/eval`) compile without an install.
            export PLTCOLLECTS=":$PWD"
            find yamd -name '*.rkt' -print0 \
              | xargs -0 raco make -j "$NIX_BUILD_CORES"
          '';
          installPhase = ''
            mkdir -p $out/share/yamd $out/bin
            cp -r yamd $out/share/yamd/yamd
            makeWrapper ${pkgs.racket}/bin/racket $out/bin/yamd \
              --add-flags "-l- yamd/cli" \
              --set PLTCOLLECTS ":$out/share/yamd" \
              --set PLT_COMPILED_FILE_CHECK exists
          '';
        };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/yamd";
        };

        # `nix fmt` — runs the same generated hook set as `nix flake check`.
        formatter =
          let
            inherit (pre-commit-check.config) package configFile;
          in
          pkgs.writeShellScriptBin "pre-commit-run" ''
            ${pkgs.lib.getExe package} run --all-files --config ${configFile}
          '';

        checks = {
          inherit pre-commit-check;

          tests =
            pkgs.runCommand "yamd-tests"
              {
                nativeBuildInputs = [ pkgs.racket ];
                src = self;
              }
              ''
                export HOME=$TMPDIR
                cp -r $src source && chmod -R +w source && cd source
                raco pkg install --auto --batch --link yamd/ || true
                raco test tests/ yamd/
                touch $out
              '';
        };
      }
    );
}
