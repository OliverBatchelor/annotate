{}:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    server = ./server;
    client = ./client;
    stm-persist = ./stm-persist;
    reflex-html = ./reflex-html;
    generic-lens-labels = ./generic-lens-labels;
  };

  shells = {
    ghc = ["common" "server" "client"];
    ghcjs = ["common" "client"];
  };
})
