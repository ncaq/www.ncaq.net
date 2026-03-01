{
  description = "static site generate for www.ncaq.net by Hakyll";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs-2511.follows = "nixpkgs";
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
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
      haskellNix,
      pyproject-nix,
      uv2nix,
      pyproject-build-systems,
      html-tidy-src,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # 公式リリースがしばらくないのでGitHubの最新版を利用。
            html-tidy = prev.html-tidy.overrideAttrs (_oldAttrs: {
              src = html-tidy-src;
            });

            # JavaScriptパッケージを管理
            npmDeps = prev.importNpmLock {
              npmRoot = final.lib.fileset.toSource {
                root = ./.;
                fileset = final.lib.fileset.unions [
                  ./package.json
                  ./package-lock.json
                ];
              };
            };
            nodeEnv = prev.buildNpmPackage {
              pname = "www-ncaq-net";
              version = "0.1.1.0";
              src = ./.;
              nodejs = final.nodejs_24;
              npmDeps = final.npmDeps;
              inherit (prev.importNpmLock) npmConfigHook;
              dontNpmBuild = true;
              # devDependenciesのsass, prettierなども含める
              npmFlags = [ "--include=dev" ];
              # CLIツールをbinディレクトリに公開
              postInstall = ''
                mkdir -p $out/bin
                ln -s $out/lib/node_modules/www-ncaq-net/node_modules/.bin/sass $out/bin/sass
                ln -s $out/lib/node_modules/www-ncaq-net/node_modules/.bin/prettier $out/bin/prettier
                ln -s $out/lib/node_modules/www-ncaq-net/node_modules/.bin/wrangler $out/bin/wrangler
              '';
            };
            nodeEnvLint = prev.buildNpmPackage {
              name = "www-ncaq-net-lint";
              src = ./.;
              nodejs = final.nodejs_24;
              npmDeps = final.npmDeps;
              inherit (prev.importNpmLock) npmConfigHook;
              npmBuildScript = "lint";
              dontNpmInstall = true;
              installPhase = ''
                runHook preInstall
                mkdir -p $out
                touch $out/lint-passed
                runHook postInstall
              '';
            };

            # uv2nixでPythonパッケージを管理
            pythonWorkspace = uv2nix.lib.workspace.loadWorkspace {
              workspaceRoot = final.lib.fileset.toSource {
                root = ./.;
                fileset = final.lib.fileset.unions [
                  ./pyproject.toml
                  ./uv.lock
                ];
              };
            };
            pythonOverlay = final.pythonWorkspace.mkPyprojectOverlay {
              sourcePreference = "wheel";
            };
            pythonBase = prev.callPackage pyproject-nix.build.packages {
              python = final.python312;
            };
            pythonSet = final.pythonBase.overrideScope (
              prev.lib.composeManyExtensions [
                pyproject-build-systems.overlays.default
                final.pythonOverlay
              ]
            );
            pythonEnv = final.pythonSet.mkVirtualEnv "www-ncaq-net-python-env" final.pythonWorkspace.deps.default;

            project = final.haskell-nix.stackProject' {
              src = final.haskell-nix.haskellLib.cleanSourceWith {
                src = ./.;
                name = "www-ncaq-net-source";
              };
              name = "www-ncaq-net";
              modules = [
                {
                  packages.www-ncaq-net.components.exes.www-ncaq-net = {
                    # Hakyllは実行時にsass, html-tidy等の外部コマンドを呼び出すため、
                    # makeWrapperでPATHに追加する。
                    postInstall = ''
                      wrapProgram $out/bin/www-ncaq-net \
                        --prefix PATH : ${
                          final.lib.makeBinPath [
                            final.html-tidy
                            final.nodeEnv
                            final.pythonEnv
                          ]
                        } \
                        --set NODE_PATH ${final.nodeEnv}/lib/node_modules/www-ncaq-net/node_modules
                    '';
                  };
                }
              ];
              shell = {
                tools = {
                  fourmolu = "latest";
                  haskell-language-server = "latest";
                  hlint = "latest";
                  stack = "latest";
                };
                buildInputs = with final; [
                  # Haskell
                  (writeScriptBin "haskell-language-server-wrapper" ''
                    #!${stdenv.shell}
                    exec haskell-language-server "$@"
                  '')

                  html-tidy
                  nodeEnv
                  nodejs_24
                  pythonEnv
                  uv
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
        packages = flake.packages // {
          default = flake.packages."www-ncaq-net:exe:www-ncaq-net";
        };
        formatter = treefmtEval.config.build.wrapper;
        checks =
          flake.packages # テストがないパッケージもビルドしてエラーを検出する。
          // flake.checks
          // {
            inherit (pkgs) nodeEnvLint;
            formatting = treefmtEval.config.build.check self;
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
