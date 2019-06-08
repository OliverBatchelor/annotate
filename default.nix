{}:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    server = ./server;
    client = ./client;
    stm-persist = ./stm-persist;
    reflex-html = ./reflex-html;
    #stitch = ./stitch;
  };

  shells = {
    ghc = ["common" "server" "client"];
    ghcjs = ["common" "client"];
  };
})
