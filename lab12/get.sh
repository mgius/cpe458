#!/bin/bash

mkdir -p stocks

for i in `cat stocks.txt`; do
   echo $i;
   # curl -s "http://ichart.finance.yahoo.com/table.csv?s=$i&d=4&e=24&f=2010&g=d&a=0&b=2&c=1990&ignore=.csv" | perl -pe 's/^(.*?,){4}(\d+\.\d+).*$/$2/;' > stocks/$i.csv
   curl -s "http://ichart.finance.yahoo.com/table.csv?s=$i&d=4&e=24&f=2010&g=d&a=0&b=2&c=1990&ignore=.csv" > stocks/$i.csv
done;

