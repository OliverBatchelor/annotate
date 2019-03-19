port=3000

for i in ~/storage/indexes/*.db; do  scripts/server.sh  $i --port $port ; ((port++)); done
for i in ~/storage/indexes/mum/*.db; do  scripts/server.sh  $i --port $port ; ((port++)); done

