{ reflex-platform ? import ./reflex-platform {} }:

reflex-platform.project ({ pkgs, ... }: {

  overrides = self: super: {
    bytes = pkgs.haskell.lib.dontCheck super.bytes;
    generic-lens = pkgs.haskell.lib.dontCheck super.generic-lens;
    linear = pkgs.haskell.lib.dontCheck super.linear;
    flat = pkgs.haskell.lib.dontCheck super.flat;
    jsaddle-warp = pkgs.haskell.lib.dontCheck super.jsaddle-warp;
  };

  packages = {
    common = ./common;
    client = ./client;
    server = ./server;
    reflex-html = ./reflex-html;
    stm-persist = ./stm-persist;
    jsaddle = ./jsaddle/jsaddle;
    jsaddle-warp = ./jsaddle/jsaddle-warp;
  };


  shells = {
    ghc = ["common" "client" "server"];
    ghcjs = ["common" "client"];
  };
})

