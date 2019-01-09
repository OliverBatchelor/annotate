
echo "Building client.."
./cabal-ghcjs new-build client

echo "Running closure compiler.."
OUTPUT=`find ./dist-ghcjs -type d -name client.jsexe`
google-closure-compiler $OUTPUT/all.js --warning_level=QUIET --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars  --externs=$OUTPUT/all.js.externs > html/annotate.min.js

echo "Building server.."
./cabal new-build server

echo "Copying files.."
mkdir "distribution"

cp `find ./dist-newstyle -type f -name server` distribution/
cp -r html distribution/
