#!/bin/sh

for i in 1 2 3 4 5
do

echo "Sending to lakka-$i.it.kth.se"
`scp $1 emdc@lakka-$i.it.kth.se:$2`

done