{
  description = "www.ncaq.net from Hakyll project template from stack";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        systems.follows = "flake-utils/systems";
      };
    };
    corepack = {
      url = "github:SnO2WMaN/corepack-flake";
      inputs.flake-utils.follows = "flake-utils";
    };
    html-tidy-src = {
      url = "github:htacg/tidy-html5";
      flake = false;
    };
  };

  outputs =
    { nixpkgs, flake-utils, haskellNix, poetry2nix, corepack, html-tidy-src, ... }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        overlays = [
          haskellNix.overlay
          poetry2nix.overlays.default
          corepack.overlays.default
          (final: prev: {
            # 公式リリースがしばらくないのでGitHubの最新版を利用。
            html-tidy =
              prev.html-tidy.overrideAttrs (oldAttrs: { src = html-tidy-src; });

            # Poetry2nixでPythonパッケージを管理
            pythonEnv = final.poetry2nix.mkPoetryEnv {
              projectDir = ./.;
              python = final.python312;
              preferWheels = true;
              overrides = final.poetry2nix.overrides.withDefaults (self: super: {
                jsx-lexer = super.jsx-lexer.overridePythonAttrs (old: {
                  buildInputs = (old.buildInputs or [ ]) ++ [ self.setuptools ];
                });
              });
            };

            project = final.haskell-nix.stackProject' {
              src = final.haskell-nix.haskellLib.cleanSourceWith {
                src = ./.;
                name = "www-ncaq-net-source";
              };
              name = "www-ncaq-net";
              compiler-nix-name = "ghc966";
              shell.tools = {
                fourmolu = "latest";
                haskell-language-server = "latest";
                hlint = "latest";
                stack = "latest";
              };
              shell.buildInputs = with pkgs; [
                # Haskell
                (pkgs.writeScriptBin "haskell-language-server-wrapper" ''
                  #!${pkgs.stdenv.shell}
                  exec haskell-language-server "$@"
                '')

                # JavaScript
                nodejs
                (mkCorepack { pm = "yarn"; })

                # Python
                pythonEnv

                # Other
                html-tidy
              ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.project.flake { };
      in flake // {
        packages = flake.packages // {
          default = flake.packages."www-ncaq-net:exe:www-ncaq-net";
        };
      });

  nixConfig = {
    extra-substituters = [ "https://cache.nixos.org" "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
  };
}
