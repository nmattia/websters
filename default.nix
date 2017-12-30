let
  bootstrap = import <nixpkgs> { };

  pkgs = (import (bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "810ec3f8bd573f437c850935de707c4025cb57db";
    sha256 = "0bv5wbvnii369kaxlwmlqr3cd3y2xjff22lhwlal37fcbkvd5nvb";
  }) {});

  ghcjs = pkgs.haskell.packages.ghcjsHEAD.override {
      overrides = self: super:
      let
        jsaddle-all =
            {
              owner = "ghcjs";
              repo = "jsaddle";
              rev = "c3b5d55afddc36f99e13ffa379b18901eade60d0";
              sha256 = "18xqq657m01zlw0awz671rk56m11a1y3ay9crgfd7920nwc5pfs6";
            };
        #jsaddle-all =
          #(import (pkgs.fetchFromGitHub
            #{
              #owner = "ghcjs";
              #repo = "jsaddle";
              #rev = "c3b5d55afddc36f99e13ffa379b18901eade60d0";
              #sha256 = "18xqq657m01zlw0awz671rk56m11a1y3ay9crgfd7920nwc5pfs6";
            #}) self );
        reflex-dom-all =
          (import (pkgs.fetchFromGitHub
            {
              owner = "reflex-frp";
              repo = "reflex-dom";
              rev = "585d49e5b79914e699cc73401299631380e49e3b";
              sha256 = "184x5fj7dzcrhwbzgm3gz7fk1kzbrjw8wrraaba9rsw1h48wq22p";
            }) self pkgs );
        packageThis = src:
          let
            origin = with src; { inherit owner repo rev sha256; };
            args =
              pkgs.lib.concatMapStringsSep " " (s: "\"${s}\"") src.args;
          in pkgs.stdenv.mkDerivation {
            name = "packaged-${origin.owner}-${origin.repo}";
            src = null;
            builder = pkgs.writeScript "dummy" ''
              set -x
              export PATH=${pkgs.mktemp}/bin:$PATH
              # Ideally we'd pass the repo here directly but I'm unable to
              # figure out the correct spell with cabal2nix version 2.2.1
              ${pkgs.cabal2nix}/bin/cabal2nix \
                ${args} \
                --revision ${origin.rev} \
                'http://github.com/${origin.owner}/${origin.repo}' 
                #> $out
              '';
          };

      in rec
      {
        ## Dependencies
        servant-reflex =
          self.callPackage
            (packageThis
              ({
                owner = "imalsogreg";
                repo = "servant-reflex";
                rev = "4f77a91a35ddd89c0ac2ecefb1c1e115ad86c460";
                sha256 = "100ga6sd316zx07l2dxn269kxa0b16xsir08wv2h7y93apiraabj";
                args = ["--compiler=ghcjs"];
              })
            ) {};
        reflex =
          self.callPackage
            (packageThis
              {
                owner = "reflex-frp";
                repo = "reflex";
                rev = "d9ef8457dfd140fde32d15a6959a15ddcd7e15e4";
                sha256 = "0946817b2q1jkw50hb4fm04c4siq94di2y042ajrdbaqz0qlq6cm";
                args = ["--compiler=ghcjs"];
              }
            ) {};
        reflex-dom-core = reflex-dom-all.reflex-dom-core;
        reflex-dom = reflex-dom-all.reflex-dom;
        ghcjs-base =
          self.callPackage
            (packageThis
              {
                owner = "ghcjs";
                repo = "ghcjs-base";
                rev = "eacf95aac3061275699563e1802eabe4a8f4aaec";
                sha256 = "096xdjp53kg1cpmc6qncjfhd8fwnbmjagpxzdd5w101pkiknd1x6";
                args = ["--compiler=ghcjs"];
              }
            ) {};
        jsaddle =
          self.callPackage
            (packageThis
              (jsaddle-all //
                { args = ["--subpath=jsaddle" "--compiler=ghcjs"];})
            ) {};
        #jsaddle = jsaddle-all.jsaddle;
        # This is only used by non-ghcjs reflex code, so it doesn't really
        # matter what the value is
        unliftio-core = null;

        ## Our package
        wzuri-interface = self.callPackage ./interface/default.nix { } ;
      };
  };

  ghc = pkgs.haskellPackages.override {
      overrides = self: super: rec {
        wzuri-interface = self.callPackage ./interface/default.nix { } ;
      };
  };

  frontend = ghcjs.callPackage ./frontend/default.nix { };
  backend = ghc.callPackage ./backend/default.nix { } ;
  server-exe = pkgs.writeScript "server"
    ''${backend}/bin/wzuri-backend ${frontend}/bin/wzuri-frontend.jsexe'';

  serve-slides = pkgs.writeScript "slides"
    ''${pkgs.haskellPackages.wai-app-static}/bin/warp -d ${./slides}'';
  foo = pkgs.cabal2nix;

in { inherit foo frontend backend server-exe serve-slides; }
