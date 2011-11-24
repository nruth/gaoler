#!/bin/sh

for i in 1 2 3 4 5
do

echo "Executing '$1' on lakka-$i.it.kth.se"
`ssh emdc@lakka-$i.it.kth.se $1`

done