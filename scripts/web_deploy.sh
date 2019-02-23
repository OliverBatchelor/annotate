
echo "Building client.."
./cabal-ghcjs new-build client

echo "Running closure compiler.."
OUTPUT=`find ./dist-ghcjs -type d -name client.jsexe`
google-closure-compiler $OUTPUT/all.js --warning_level=QUIET --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars  --externs=$OUTPUT/all.js.externs > html/annotate.min.js

echo "Building server.."
cabal new-build server --project-file=server.project

echo "Copying files.."
mkdir -p "distribution"

cp `find ./dist-newstyle -type f -name server` distribution/
cp -r html distribution/
cp scripts/run_restarting.sh distribution/


echo "Deploying and restarting.."
#ssh cs18018hr killall server
#ssh cs15071kn killall server

#scp -r distribution/* cs18018hr:annotate/

#ssh oliver-home killall server
#scp -r distribution/* oliver-home:annotate/


