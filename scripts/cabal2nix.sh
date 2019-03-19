for path in client common server reflex-html stm-persist;
  do
    cabal2nix ./$path > ./$path/default.nix
  done

