echo "Deploying to $1.."


ssh $1 "mv --backup=t annotate .backup"

scp -r distribution $1:annotate
#scp -r ~/sync/detection $1:detection

#ssh oliver-home killall server
#scp -r distribution/* oliver-home:annotate/


