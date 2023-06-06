#!/usr/bin/env bash

rm kekel.txt
for i in {1..10000}
do
    from=$(jot -r 1 100000)
    to=$(jot -r 1 "$from" 100010)
    price=$(jot  -p2 -r 1 0 250)
    echo "[$from-$to: A=$price]," >> kekel.txt
done