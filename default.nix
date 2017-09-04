let
  bootstrap = import <nixpkgs> { };

  pkgs = (import (bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "ce8802729430c21dd4b437f8441f278a869e4a44";
    sha256 = "1xa6wv58csv5d1smppcvvddyz07gwa6lsirblz5fps5gr1phcibg";
  }) {});

  ghcjs = (import (pkgs.fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "1670c5b899658babeda58329d3df6b943cf6aeca";
    sha256 = "0ry3fcxiqr43c5fghsiqn0iarj4gfvk77jkc4na7j7r3k8vjdjh2";
  }) {}).ghcjs;

  addInterface = compiler: compiler.override {
      overrides = self: super: rec {
        wzuri-interface = self.callPackage ./interface/default.nix { } ;
        };
    };

  ghcWithInterface = addInterface pkgs.haskellPackages;
  ghcjsWithInterface = addInterface ghcjs;

  frontend = ghcjsWithInterface.callPackage ./frontend/default.nix { };
  backend = ghcWithInterface.callPackage ./backend/default.nix { } ;
  server-exe = pkgs.writeScript "server"
      ''
      #!/usr/bin/env bash

      ${backend}/bin/wzuri-backend ${frontend}/bin/wzuri-frontend.jsexe
      '';

in { inherit frontend backend server-exe; }
