{ reflex-platform ? import ./reflex-platform {} }:

reflex-platform.project ({ pkgs, ... }: {

  overrides = self: super: {
    bytes = pkgs.haskell.lib.dontCheck super.bytes;
    generic-lens = pkgs.haskell.lib.dontCheck super.generic-lens;
    linear = pkgs.haskell.lib.dontCheck super.linear;
    flat = pkgs.haskell.lib.dontCheck super.flat;
  };

  packages = {
    common = ./common;
    client = ./client;
    server = ./server;
    reflex-html = ./reflex-html;
    stm-persist = ./stm-persist;
    record-hasfield = ./record-hasfield;
  };


  shells = {
    ghc = ["common" "client" "server"];
    ghcjs = ["common" "client"];
  };
})

