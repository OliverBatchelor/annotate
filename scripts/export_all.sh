for i in ~/storage/indexes/*.db; do filename=$(basename "$i" .db); scripts/server.sh  $i --export ~/storage/export/$filename.json ; done


for i in ~/storage/indexes/dad/*.db; do filename=$(basename "$i" .db); scripts/server.sh  $i --export ~/storage/export/dad/$filename.json ; done
for i in ~/storage/indexes/oliver/*.db; do filename=$(basename "$i" .db); scripts/server.sh  $i --export ~/storage/export/oliver/$filename.json ; done
for i in ~/storage/indexes/mum/*.db; do filename=$(basename "$i" .db); scripts/server.sh  $i --export ~/storage/export/mum/$filename.json ; done
