{
  description = "static site generator for www.ncaq.net by Hakyll";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pyproject-nix = {
      url = "github:pyproject-nix/pyproject.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    uv2nix = {
      url = "github:pyproject-nix/uv2nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        pyproject-nix.follows = "pyproject-nix";
      };
    };
    pyproject-build-systems = {
      url = "github:pyproject-nix/build-system-pkgs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        pyproject-nix.follows = "pyproject-nix";
        uv2nix.follows = "uv2nix";
      };
    };
    html-tidy-src = {
      url = "github:htacg/tidy-html5";
      flake = false;
    };
  };

  outputs =
    inputs@{
      flake-parts,
      treefmt-nix,
      pyproject-nix,
      uv2nix,
      pyproject-build-systems,
      html-tidy-src,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      imports = [
        treefmt-nix.flakeModule
      ];

      perSystem =
        { pkgs, lib, ... }:
        let
          # 公式リリースがしばらくないのでGitHubの最新版を利用。
          html-tidy = pkgs.html-tidy.overrideAttrs (_oldAttrs: {
            src = html-tidy-src;
          });

          # JavaScriptパッケージを管理
          inherit (pkgs) nodejs; # nixpkgsのstableのバージョンを基本的に利用。
          npmRoot = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./package-lock.json
              ./package.json
            ];
          };
          nodeModules = pkgs.importNpmLock.buildNodeModules {
            inherit
              nodejs
              npmRoot
              ;
          };
          # npmツールが参照するソース。
          npmSrc = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./.editorconfig
              ./.gitignore
              ./.prettierignore
              ./package-lock.json
              ./package.json
              ./site
              ./stylelint.config.ts
              ./tsconfig.json
              ./vite.config.ts
            ];
          };
          # npm run経由でスクリプト実行を簡単にするためのヘルパー。
          mkNpmCheck =
            name: script:
            pkgs.runCommand name
              {
                nativeBuildInputs = [ nodejs ];
              }
              ''
                cp -r ${npmSrc}/. .
                ln -s ${nodeModules}/node_modules node_modules
                npm run ${script}
                touch $out
              '';

          # uv2nixでPythonパッケージを管理
          pythonWorkspace = uv2nix.lib.workspace.loadWorkspace {
            workspaceRoot = pkgs.lib.fileset.toSource {
              root = ./.;
              fileset = pkgs.lib.fileset.unions [
                ./pyproject.toml
                ./uv.lock
              ];
            };
          };
          pythonOverlay = pythonWorkspace.mkPyprojectOverlay {
            sourcePreference = "wheel";
          };
          pythonBase = pkgs.callPackage pyproject-nix.build.packages {
            python = pkgs.python312;
          };
          pythonSet = pythonBase.overrideScope (
            pkgs.lib.composeManyExtensions [
              pyproject-build-systems.overlays.default
              pythonOverlay
            ]
          );
          pythonEnv = pythonSet.mkVirtualEnv "www-ncaq-net-python-env" pythonWorkspace.deps.default;

          # Haskellビルド
          www-ncaq-net-unwrapped =
            pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "www-ncaq-net" ./. { })
              {
                # cabal buildでzlibのC依存を解決するために必要。
                executablePkgconfigDepends = with pkgs; [ zlib ];
              };
          www-ncaq-net = www-ncaq-net-unwrapped.overrideAttrs (oldAttrs: {
            nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
            # Hakyllは実行時に外部コマンドを呼び出すため、
            # makeWrapperでPATHに追加する。
            postInstall = (oldAttrs.postInstall or "") + ''
              wrapProgram $out/bin/www-ncaq-net \
                --prefix PATH : ${
                  pkgs.lib.makeBinPath [
                    pkgs.nodejs
                    pkgs.uv
                    pkgs.wrangler

                    html-tidy
                    pythonEnv
                  ]
                } \
                --set NODE_PATH ${nodeModules}/node_modules
            '';
          });
        in
        {
          treefmt.config = {
            # yamlfmtはprettierと競合する。
            projectRootFile = "flake.nix";
            programs = {
              actionlint.enable = true;
              cabal-gild.enable = true;
              deadnix.enable = true;
              fourmolu.enable = true;
              hlint.enable = true;
              nixfmt.enable = true;
              prettier.enable = true;
              shellcheck.enable = true;
              shfmt.enable = true;
              statix.enable = true;
              typos.enable = true;
              zizmor.enable = true;
            };
            settings.formatter = {
              editorconfig-checker = {
                command = pkgs.editorconfig-checker;
                includes = [ "*" ];
              };
              zizmor.options = [ "--pedantic" ];
            };
          };

          # テストがないパッケージもビルドしてエラーを検出する。
          checks = {
            inherit www-ncaq-net;
            build = mkNpmCheck "build" "build";
            lint-prettier = mkNpmCheck "lint-prettier" "lint:prettier";
            lint-stylelint = mkNpmCheck "lint-stylelint" "lint:stylelint";
            lint-tsc = mkNpmCheck "lint-tsc" "lint:tsc";
          };

          packages = {
            default = www-ncaq-net;
            # flake.lockの管理バージョンをre-exportすることで安定した利用を促進。
            inherit (pkgs) nix-fast-build;
          };

          devShells.default = pkgs.mkShell {
            inputsFrom = [ www-ncaq-net-unwrapped ];
            packages = with pkgs; [
              # treefmtで指定したプログラムの単体版。
              actionlint
              deadnix
              editorconfig-checker
              fourmolu
              haskellPackages.cabal-gild
              hlint
              nixfmt
              prettier
              shellcheck
              shfmt
              statix
              typos
              zizmor

              # nixの関連ツール。
              nil
              nix-fast-build

              # GitHub関連ツール。
              gh

              # Haskell関連ツール。
              cabal-install
              haskell-language-server

              # JavaScript関連ツール。
              importNpmLock.hooks.linkNodeModulesHook
              nodejs

              # Python関連ツール。
              pythonEnv
              uv

              # その他のツール。
              html-tidy
              wrangler
            ];
            npmDeps = pkgs.importNpmLock.buildNodeModules {
              inherit (pkgs) nodejs;
              inherit npmRoot;
            };
          };
        };
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org/"
      "https://niks3-public.ncaq.net/"
      "https://ncaq.cachix.org/"
      "https://nix-community.cachix.org/"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "niks3-public.ncaq.net-1:e/B9GomqDchMBmx3IW/TMQDF8sjUCQzEofKhpehXl04="
      "ncaq.cachix.org-1:XF346GXI2n77SB5Yzqwhdfo7r0nFcZBaHsiiMOEljiE="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
