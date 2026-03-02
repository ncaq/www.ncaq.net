{
  description = "static site generator for www.ncaq.net by Hakyll";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
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
          packages = {
            default = www-ncaq-net;
          };

          # テストがないパッケージもビルドしてエラーを検出する。
          checks = {
            inherit www-ncaq-net;
          };

          treefmt = {
            # yamlfmtはprettierと競合する。
            projectRootFile = "flake.nix";
            programs = {
              actionlint.enable = true;
              cabal-gild.enable = true;
              deadnix.enable = true;
              fourmolu.enable = true;
              hlint.enable = true;
              nixfmt.enable = true;
              shellcheck.enable = true;
              shfmt.enable = true;
              statix.enable = true;

              prettier = {
                enable = true;
                excludes = [ "*.md" ];
              };
            };
          };

          devShells.default = pkgs.mkShell {
            inputsFrom = [ www-ncaq-net-unwrapped ];
            packages = with pkgs; [
              cabal-install
              fourmolu
              haskell-language-server
              hlint

              html-tidy

              importNpmLock.hooks.linkNodeModulesHook
              nodeEnv
              nodejs

              pythonEnv
              uv
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
      "https://www-ncaq-net.cachix.org"
    ];
    extra-trusted-public-keys = [
      "www-ncaq-net.cachix.org-1:muU00ItcyCf+F+lS//4w9XxW+UZYQxGa1cra1VXRH8c="
    ];
  };
}
