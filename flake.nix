{
  description = "www.ncaq.net from Hakyll project template from stack";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-2511";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    haskellNix.url = "github:input-output-hk/haskell.nix";
    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        systems.follows = "flake-utils/systems";
        treefmt-nix.follows = "treefmt-nix";
      };
    };
    html-tidy-src = {
      url = "github:htacg/tidy-html5";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
      haskellNix,
      poetry2nix,
      html-tidy-src,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        nodeEnv-npmDeps = pkgs.importNpmLock { npmRoot = ./.; };
        overlays = [
          haskellNix.overlay
          poetry2nix.overlays.default
          (final: prev: {
            # 公式リリースがしばらくないのでGitHubの最新版を利用。
            html-tidy = prev.html-tidy.overrideAttrs (_oldAttrs: {
              src = html-tidy-src;
            });

            # JavaScriptパッケージを管理
            nodeEnv = pkgs.buildNpmPackage {
              pname = "www-ncaq-net";
              version = "0.1.1.0";
              src = ./.;
              npmDeps = nodeEnv-npmDeps;
              inherit (pkgs.importNpmLock) npmConfigHook;
            };
            nodeEnv-lint = pkgs.buildNpmPackage {
              name = "www-ncaq-net-lint";
              src = ./.;
              npmDeps = nodeEnv-npmDeps;
              inherit (pkgs.importNpmLock) npmConfigHook;
              npmBuildScript = "lint";
              dontNpmInstall = true;
              installPhase = ''
                runHook preInstall
                mkdir -p $out
                touch $out/lint-passed
                runHook postInstall
              '';
            };

            # Poetry2nixでPythonパッケージを管理
            pythonEnv = final.poetry2nix.mkPoetryEnv {
              projectDir = ./.;
              python = final.python312;
              preferWheels = true;
              overrides = final.poetry2nix.overrides.withDefaults (
                self: super: {
                  jsx-lexer = super.jsx-lexer.overridePythonAttrs (old: {
                    buildInputs = (old.buildInputs or [ ]) ++ [ self.setuptools ];
                  });
                }
              );
            };

            project = final.haskell-nix.stackProject' {
              src = final.haskell-nix.haskellLib.cleanSourceWith {
                src = ./.;
                name = "www-ncaq-net-source";
              };
              name = "www-ncaq-net";
              shell = {
                tools = {
                  fourmolu = "latest";
                  haskell-language-server = "latest";
                  hlint = "latest";
                  stack = "latest";
                };
                buildInputs = with pkgs; [
                  # Haskell
                  (pkgs.writeScriptBin "haskell-language-server-wrapper" ''
                    #!${pkgs.stdenv.shell}
                    exec haskell-language-server "$@"
                  '')

                  # JavaScript
                  nodejs

                  # Python
                  pythonEnv

                  # Other
                  html-tidy
                ];
              };
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.project.flake { };
        treefmtEval = treefmt-nix.lib.evalModule pkgs (_: {
          # actionlintはセルフホストランナーの設定ファイルを正常に読み込まなかった。
          # yamlfmtはprettierと競合する。
          projectRootFile = "flake.nix";
          programs = {
            cabal-gild.enable = true;
            deadnix.enable = true;
            hlint.enable = true;
            nixfmt.enable = true;
            shellcheck.enable = true;
            shfmt.enable = true;
            statix.enable = true;

            fourmolu = {
              enable = true;
              package = pkgs.fourmolu;
            };
            prettier = {
              enable = true;
              excludes = [ "*.md" ];
            };
          };
        });
      in
      # hydraJobsもGitHub Actionsを使うため不要なので除外。
      builtins.removeAttrs flake [
        "ciJobs"
        "hydraJobs"
      ]
      // {
        checks =
          flake.packages # テストがないパッケージもビルドしてエラーを検出する。
          // flake.checks
          // {
            inherit (pkgs) nodeEnv-lint;
            formatting = treefmtEval.config.build.check self;
          };
        formatter = treefmtEval.config.build.wrapper;
        packages = flake.packages // {
          default = flake.packages."www-ncaq-net:exe:www-ncaq-net";
        };
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://cache.iog.io"
      "https://www-ncaq-net.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "www-ncaq-net.cachix.org-1:muU00ItcyCf+F+lS//4w9XxW+UZYQxGa1cra1VXRH8c="
    ];
    allow-import-from-derivation = true;
  };
}
