./cabal-ghcjs new-build client 


OUTPUT=`find ./dist-ghcjs -type d -name client.jsexe`
google-closure-compiler $OUTPUT/all.js --warning_level=QUIET --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars  --externs=$OUTPUT/all.js.externs > html/annotate.min.js

zopfli html/annotate.min.js
