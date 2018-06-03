cabal new-build client --ghcjs


OUTPUT=`find ./dist-newstyle -type d -name client.jsexe`
ccjs $OUTPUT/all.js --warning_level=QUIET --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --externs=node --externs=$OUTPUT/all.js.externs > html/annotate.min.js

zopfli html/annotate.min.js
