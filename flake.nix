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
        { pkgs, ... }:
        let
          # 公式リリースがしばらくないのでGitHubの最新版を利用。
          html-tidy = pkgs.html-tidy.overrideAttrs (_oldAttrs: {
            src = html-tidy-src;
          });

          # JavaScriptパッケージを管理
          npmRoot = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./package.json
              ./package-lock.json
            ];
          };
          npmDeps = pkgs.importNpmLock {
            inherit npmRoot;
          };
          nodeEnv = pkgs.buildNpmPackage {
            pname = "www-ncaq-net";
            version = "0.1.1.0";
            src = npmRoot;
            inherit npmDeps;
            inherit (pkgs.importNpmLock) npmConfigHook;
            dontNpmBuild = true;
            # devDependenciesのsass, prettierなども含める
            npmFlags = [ "--include=dev" ];
          };

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
            # Hakyllは実行時にsass, html-tidy等の外部コマンドを呼び出すため、
            # makeWrapperでPATHに追加する。
            postInstall = (oldAttrs.postInstall or "") + ''
              wrapProgram $out/bin/www-ncaq-net \
                --prefix PATH : ${
                  pkgs.lib.makeBinPath [
                    html-tidy
                    nodeEnv
                    pkgs.nodejs
                    pkgs.uv
                    pythonEnv
                  ]
                } \
                --set NODE_PATH ${nodeEnv}/lib/node_modules/www-ncaq-net/node_modules
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
              fourmolu
              haskell-language-server
              hlint

              # JavaScript関連ツール。
              importNpmLock.hooks.linkNodeModulesHook
              nodeEnv
              nodejs

              # Python関連ツール。
              pythonEnv
              uv

              # その他のツール。
              html-tidy
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
