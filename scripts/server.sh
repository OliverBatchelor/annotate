
#./cabal new-build server && `find ./dist-newstyle -type f -name server`  $@
cabal new-build server --project-file=server.project && `find ./dist-newstyle -type f -name server`  $@
